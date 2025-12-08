#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <stdlib.h>

// Convert base-32 string to integer
static int parse_base32_(const char* str, int len) {
    int result = 0;
    int is_negative = 0;
    int start = 0;

    if (str[0] == '-') {
        is_negative = 1;
        start = 1;
    }

    for (int i = start; i < len; i++) {
        result *= 32;
        char c = str[i];

        if (c >= '0' && c <= '9') {
            result += c - '0';
        } else if (c >= 'a' && c <= 'v') {
            result += c - 'a' + 10;
        } else if (c >= 'A' && c <= 'V') {
            result += c - 'A' + 10;
        }
    }

    return is_negative ? -result : result;
}

// Extract values from compressed string using pattern ([+-])([^+-]+)
static SEXP extract_values_(const char* part) {
    int capacity = 1024;
    int count = 0;
    char** values = (char**)malloc(capacity * sizeof(char*));

    const char* p = part;
    while (*p) {
        if (*p == '+' || *p == '-') {
            const char* start = p;
            char sign = *p;
            p++;

            // Find end of value (next +/- or end of string)
            const char* end = p;
            while (*end && *end != '+' && *end != '-') {
                end++;
            }

            int len = end - p;
            if (len > 0) {
                // Allocate space for value (with potential minus sign)
                int val_len = (sign == '-') ? len + 1 : len;
                char* val = (char*)malloc((val_len + 1) * sizeof(char));

                if (sign == '-') {
                    val[0] = '-';
                    strncpy(val + 1, p, len);
                    val[len + 1] = '\0';
                } else {
                    strncpy(val, p, len);
                    val[len] = '\0';
                }

                if (count >= capacity) {
                    capacity *= 2;
                    values = (char**)realloc(values, capacity * sizeof(char*));
                }

                values[count++] = val;
                p = end;
            }
        } else {
            p++;
        }
    }

    // Convert to R character vector
    SEXP result = PROTECT(allocVector(STRSXP, count));
    for (int i = 0; i < count; i++) {
        SET_STRING_ELT(result, i, mkChar(values[i]));
        free(values[i]);
    }
    free(values);

    UNPROTECT(1);
    return result;
}

// Decode XY coordinates
static SEXP decode_xy_(const char* part, int* n_points) {
    SEXP values = PROTECT(extract_values_(part));
    int n_values = length(values);

    if (n_values < 1) {
        UNPROTECT(1);
        *n_points = 0;
        return R_NilValue;
    }

    // Parse factor
    const char* factor_str = CHAR(STRING_ELT(values, 0));
    int factor = parse_base32_(factor_str, strlen(factor_str));

    // Calculate number of points
    *n_points = (n_values - 1) / 2;

    // Create matrix for points (n_points x 2)
    SEXP points = PROTECT(allocMatrix(REALSXP, *n_points, 2));
    double* points_ptr = REAL(points);

    int difference_x = 0;
    int difference_y = 0;

    for (int i = 0; i < *n_points; i++) {
        const char* x_str = CHAR(STRING_ELT(values, i * 2 + 1));
        const char* y_str = CHAR(STRING_ELT(values, i * 2 + 2));

        int x = parse_base32_(x_str, strlen(x_str));
        int y = parse_base32_(y_str, strlen(y_str));

        difference_x += x;
        difference_y += y;

        points_ptr[i] = (double)difference_x / factor;
        points_ptr[*n_points + i] = (double)difference_y / factor;
    }

    UNPROTECT(2);
    return points;
}

// Decode additional dimension (Z or M)
static void decode_additional_(SEXP points, const char* part, int col_idx) {
    SEXP values = PROTECT(extract_values_(part));
    int n_values = length(values);

    if (n_values < 1) {
        UNPROTECT(1);
        return;
    }

    const char* factor_str = CHAR(STRING_ELT(values, 0));
    int factor = parse_base32_(factor_str, strlen(factor_str));

    int n_points = nrows(points);
    double* points_ptr = REAL(points);

    int difference = 0;
    for (int i = 1; i < n_values && (i - 1) < n_points; i++) {
        const char* val_str = CHAR(STRING_ELT(values, i));
        int val = parse_base32_(val_str, strlen(val_str));

        difference += val;
        points_ptr[col_idx * n_points + (i - 1)] = (double)difference / factor;
    }

    UNPROTECT(1);
}

// Decode single geometry string
static SEXP decode_single_geometry_(const char* geometry) {

    // Split by '|'
    int n_parts = 1;
    for (const char* p = geometry; *p; p++) {
        if (*p == '|') n_parts++;
    }

    char** parts = (char**)malloc(n_parts * sizeof(char*));
    const char* start = geometry;
    int part_idx = 0;

    for (const char* p = geometry; ; p++) {
        if (*p == '|' || *p == '\0') {
            int len = p - start;
            parts[part_idx] = (char*)malloc((len + 1) * sizeof(char));
            strncpy(parts[part_idx], start, len);
            parts[part_idx][len] = '\0';
            part_idx++;
            start = p + 1;

            if (*p == '\0') break;
        }
    }

    const char* first = parts[0];
    SEXP result;

    // Check if old or new version
    if (strncmp(first, "+0", 2) != 0) {
        // Old version
        int n_points;
        result = PROTECT(decode_xy_(first, &n_points));
    } else {
        // New version
        if (strlen(first) < 6) {
            error("Invalid compressed geometry format");
        }

        int flag = first[5] - '0';
        int version = first[3] - '0';

        if (version != 1) {
            error("Compressed geometry: Unexpected version");
        }

        if (flag > 3) {
            error("Compressed geometry: Invalid flags");
        }

        const char* xy_part = first + 6;
        int n_points;
        SEXP xy_matrix = PROTECT(decode_xy_(xy_part, &n_points));

        if (flag == 0) {
            result = xy_matrix;
        } else {
            // Need to add Z and/or M dimensions
            int n_cols = 2;
            if ((flag & 1) == 1) n_cols++; // Has Z
            if ((flag & 2) == 2) n_cols++; // Has M

            result = PROTECT(allocMatrix(REALSXP, n_points, n_cols));
            double* result_ptr = REAL(result);
            double* xy_ptr = REAL(xy_matrix);

            // Copy X and Y
            for (int i = 0; i < n_points; i++) {
                result_ptr[i] = xy_ptr[i];
                result_ptr[n_points + i] = xy_ptr[n_points + i];
            }

            // Decode Z if present
            if ((flag & 1) == 1 && n_parts > 1) {
                decode_additional_(result, parts[1], 2);
            }

            // Decode M if present
            if ((flag & 2) == 2 && n_parts > 1) {
                int m_col = (flag == 3) ? 3 : 2;
                decode_additional_(result, parts[n_parts - 1], m_col);
            }

            UNPROTECT(1); // xy_matrix
        }
    }

    // Free parts
    for (int i = 0; i < n_parts; i++) {
        free(parts[i]);
    }
    free(parts);

    UNPROTECT(1);
    return result;
}

// Main decode function - handles character vectors
SEXP decode_compressed_geometry_(SEXP geometry_str) {
    if (!isString(geometry_str)) {
        error("geometry must be a character vector");
    }

    int n = length(geometry_str);

    // Allocate list to store results
    SEXP result_list = PROTECT(allocVector(VECSXP, n));

    // Decode each geometry string
    for (int i = 0; i < n; i++) {
        const char* geometry = CHAR(STRING_ELT(geometry_str, i));
        SEXP decoded = PROTECT(decode_single_geometry_(geometry));
        SET_VECTOR_ELT(result_list, i, decoded);
        UNPROTECT(1);
    }

    UNPROTECT(1);
    return result_list;
}
