/SolveLocationAllocation
GET https://logistics.arcgis.com/arcgis/rest/services/World/LocationAllocation/GPServer/SolveLocationAllocation/submitJob
The /SolveLocationAllocation job request finds a set of facilities that will best serve demand from surrounding areas. As the name suggests, location-allocation is a two-fold problem that simultaneously locates facilities and allocates demand points to the facilities.


More info



Parameters
Name	Required	Type	Default	Description
f	
string		
The request response format, either json or pjson

token	
string		
An access token with the required privileges.

facilities	
feature		
One or more locations that serve as facilities.

demand_points	
feature		
One or more demand points.

travel_mode	
object		
The mode of transportation for the analysis provided as a JSON object.

locate_settings	
object		
Determines how input data are located.

measurement_units	
string	
Minutes

Units that should be used to report the total travel time or travel distance for the output routes.

analysis_region	
string		
Region in which to perform the analysis.

problem_type	
string	
Minimize Impedance

Objective of the location-allocation analysis.

number_of_facilities_to_find	
integer	
1

The number of facilities the task should choose.

default_measurement_cutoff	
number	
None

The maximum travel time or distance allowed between a demand point and the facility to which it is allocated.

default_capacity	
number	
1

The default capacity assigned to all facilities in the analysis. Only applicable to the Maximize Capacitated Coverage problem type; ignored for all other problem types.

target_market_share	
number	
10

The percentage of the total demand weight that you want the chosen and required facilities to capture. Only applicable to the Target Market Share problem type; ignored for all other problem types.

measurement_transformation_model	
string	
Linear

The equation for transforming the network cost between facilities and demand points.

measurement_transformation_factor	
number	
1

The impedance parameter value (λ) to the equation specified in the measurement_transformation_model parameter. Ignored when the impedance transformation is linear.

travel_direction	
string	
Facility to Demand

Measure travel times or distances from facilities to demand points or from demand points to facilities.

time_of_day	
datetime		
The time at which travel begins, or departs, from the starting locations.

time_zone_for_time_of_day	
string	
Geographically Local

The time zone or zones of the time_of_day parameter.

uturn_at_junctions	
string	
Allowed only at Intersections and Dead Ends

Restricts or allows a route to make U-turns at junctions.

point_barriers	
feature		
One or more points that act as temporary restrictions, additional time, or distance.

line_barriers	
feature		
One ore more lines that prohibit travel anywhere the lines intersect the streets.

polygon_barriers	
feature		
Polygons that either prohibit travel or proportionately scale the time or distance required to travel on the streets.

use_hierarchy	
boolean	
true

Hierarchy used when finding the shortest paths.

restrictions	
[string]		
The restrictions that should be honored.

attribute_parameter_values	
table		
Additional values required by an attribute or restriction.

allocation_line_shape	
string	
Straight Line

Type of line features that are output by the request.

time_impedance	
string		
Time-based impedance.

distance_impedance	
string	
Miles

Distance-based impedance.

impedance	
string		
Type of impedance.

save_output_network_analysis_layer	
boolean	
false

Save the analysis settings as a network analysis layer file.

output_format	
string	
Feature Set

Format in which the output features will be returned.

context	
object		
Additional settings that affect task operation

overrides	
string		
For internal use only.

ignore_invalid_locations	
boolean	
true

Ignores invalid input locations.

Required parameters
f
string
required
Values: json | pjson
The response format.

f=json
token
string
required
An access token with the required privileges.

ArcGIS Location Platform: premium:user:networkanalysis:locationallocation
ArcGIS Online: premium:user:networkanalysis
token=<ACCESS_TOKEN>

HTTP headers for tokens

Learn more about access tokens and privileges in the Security and authentication developer guide.

facilities
feature
required
Specify one or more locations that serve as facilities. The best facility or facilities to serve the demand points is identified by the request.

When specifying the facilities, you can set attributes for each as follows:

Show attributes for facilities
Attributes for facilities
Namestring (length: 500)nullable

The name of the facility. The name is included in the name of output allocation lines if the facility is part of the solution.

FacilityTypeint enumdefault:0

Allowed values: 0, 1, 2

Specify whether the facility is a candidate, required, or a competitor facility. The field value is specified as one of the following integers:

0:Candidate—A facility that may be part of the solution.
1:Required—A facility that must be part of the solution.
2:Competitor—A rival facility that potentially removes demand from your facilities. Competitor facilities are specific to the maximize market share and target market share problem types; they are ignored in other problem types.
Weightnumber (non-negative)

The relative weighting of the facility, which is used to rate the attractiveness, desirability, or bias of one facility compared to another.

For example, a value of 2.0 could capture the preference of customers who prefer, at a ratio of 2 to 1, shopping in one facility over another facility. Factors that potentially affect facility weight include square footage, neighborhood, and age of the building. Weight values other than one are only honored by the maximize market share and target market share problem types; they are ignored in other problem types.

Cutoffnumber (non-negative)default:0

The impedance value at which to stop searching for demand points from a given facility. The demand point can't be allocated to a facility that is beyond the value indicated here.

This attribute allows you to specify a different cutoff value for each demand point. For example, You might find that people in rural areas are willing to travel up to 10 miles to reach a facility, while urbanites are only willing to travel up to 2 miles. You can model this behavior by setting the Cutoff value for all demand points that are in rural areas to 10 and setting the Cutoff value of the demand points in urban areas to 2.

Capacitynumber (non-negative)default:0

The Capacity field is specific to the Maximize Capacitated Coverage problem type; the other problem types ignore this field.

Capacity specifies how much weighted demand the facility is capable of supplying. Excess demand won't be allocated to a facility even if that demand is within the facility's default measurement cutoff.

Any value assigned to the Capacity field overrides the Default Capacity parameter (Default_Capacity in Python) for the given facility.

CurbApproachint enumdefault:0

Allowed values: 0, 1, 2, 3

Specifies the direction a vehicle may arrive at and depart from the facility. The field value is specified as one of the following integers:

0: Either side of vehicle. The vehicle can approach and depart the facility in either direction. U-turns are allowed. You should choose this setting if your vehicle can make a U-turn at the order or if it can pull into a driveway or parking lot and turn around.
1: Right side of vehicle. When the vehicle approaches and departs the facility, the curb must be on the right side of the vehicle. A U-turn is prohibited.
2: Left side of vehicle. When the vehicle approaches and departs the facility, the curb must be on the left side of the vehicle. A U-turn is prohibited.
3: No U-turn. When the vehicle approaches the facility, the curb can be on either side of the vehicle; however, the vehicle must depart without turning around. Learn more about U-turn policies
Show illustration
Bearingnumber (non-negative)nullable

The direction in which a point is moving. The units are degrees and are measured clockwise from true north.

This attribute is used in conjunction with the BearingTol attribute. Bearing data is usually sent automatically from a mobile device equipped with a GPS receiver. Try to include bearing data if you are loading an input location that is moving, such as a pedestrian or a vehicle. Using this attribute tends to prevent adding locations to the wrong edges, which can occur when a vehicle is near an intersection or an overpass, for example. Bearing also helps the solver determine the side of the street that the point is on.

Learn more about bearing and bearing tolerance

BearingTolnumber (range: 0 - 180)default:30nullable

The bearing tolerance value creates a range of acceptable bearing values when locating moving points on an edge using the Bearing attribute.

If the Bearing attribute value is within the range of acceptable values that are generated from the bearing tolerance on an edge, the point can be added as a network location there; otherwise, the closest point on the next-nearest edge is evaluated. The units are in degrees. A value of 30 means that when Network Analyst attempts to add a network location on an edge, a range of acceptable bearing values is generated 15 degrees to either side of the edge (left and right) and in both digitized directions of the edge.

Learn more about bearing and bearing tolerance

NavLatencynumber (non-negative)nullable

Indicates how much cost is expected to elapse from the moment GPS information is sent from a moving vehicle to a server and the moment the processed route is received by the vehicle's navigation device.

This attribute is only used in the solve process if the Bearing and BearingTol fields also have values; however, providing a NavLatency attribute value is optional, even when values are present in the Bearing and BearingTol.

Example

The example shows how to specify some attributes for the facilities.

{
  "features": [
    {
      "geometry": {
        "y": 51.5254,
        "x": -0.1891
      },
      "attributes": {
        "Name": "Facility 1",
        "ID": "F100045",
        "Capacity": 100,
        "CurbApproach": 0
      }
    },
    {
      "geometry": {
        "y": 51.5353,
        "x": -0.1744
      },
      "attributes": {
demand_points
feature
required
Specify one or more demand points. The request identifies the best facilities based in large part on how the facilities serve the demand points specified.

When specifying the demand points, you can set attributes for each as follows:

Show attributes for demand points
Attributes for demand points
Namestring (length: 500)nullable

The name of the demand point. The name is included in the name of an output allocation line or lines if the demand point is part of the solution.

GroupNamestring (length: 500)

The name of the group to which the demand point belongs. This field is ignored for the Maximize Capacitated Coverage, Target Market Share, and Maximize Market Share problem types.

If demand points share a group name, the solver allocates all members of the group to the same facility. (If constraints, such as a cutoff distance, prevent any of the demand points in the group from reaching the same facility, none of the demand points are allocated.)

Minimizing distance without group names
Minimize distance without grouped demand points.

Minimizing distance with group names
Minimize distance with grouped demand points. In this example, the yellow demand points have the same GroupName value, so they are allocated to the same facility.

Weightnumber (non-negative)

The relative weighting of the demand point. A value of 2.0 means the demand point is twice as important as one with a weight of 1.0. If demand points represent households, for example, weight could indicate the number of people in each household.

Cutoffnumber (non-negative)default:0

The impedance value at which to stop searching for demand points from a given facility. The demand point can't be allocated to a facility that is beyond the value indicated here.

This attribute allows you to specify a cutoff value for each demand point. For example, you may find that people in rural areas are willing to travel up to 10 miles to reach a facility, while those in urban areas are only willing to travel up to 2 miles. You can model this behavior by setting the Cutoff value for all demand points that are in rural areas to 10 and setting the Cutoff value of the demand points in urban areas to 2.

The units for this attribute value are specified by the measurement_units parameter.

A value for this attribute overrides the default set for the analysis using the default_measurement_cutoff parameter. The default value is Null, which results in the default value set by the default_measurement_cutoff parameter being used for all the demand points.

ImpedanceTransformationinteger

Override the default value set for the analysis by the measurement_transformation_model parameter.

ImpedanceParameternumber (non-negative)

Override the default value set for the analysis by the measurement_transformation_model parameter.

CurbApproachint enumdefault:0

Allowed values: 0, 1, 2, 3

Specifies the direction a vehicle may arrive at or depart from the demand point. The field value is specified as one of the following integers:

0: Either side of vehicle. The vehicle can approach and depart the demand point in either direction. U-turns are allowed. You should choose this setting if your vehicle can make a U-turn at the order or if it can pull into a driveway or parking lot and turn around.
1: Right side of vehicle. When the vehicle approaches and departs the demand point, the curb must be on the right side of the vehicle. A U-turn is prohibited.
2: Left side of vehicle. When the vehicle approaches and departs the demand point, the curb must be on the left side of the vehicle. A U-turn is prohibited.
3: No U-turn. When the vehicle approaches the demand point, the curb can be on either side of the vehicle; however, the vehicle must depart without turning around. Learn more about U-turn policies
Show illustration
Bearingnumber (non-negative)nullable

The direction in which a point is moving. The units are degrees and are measured clockwise from true north.

This attribute is used in conjunction with the BearingTol attribute. Bearing data is usually sent automatically from a mobile device equipped with a GPS receiver. Try to include bearing data if you are loading an input location that is moving, such as a pedestrian or a vehicle. Using this attribute tends to prevent adding locations to the wrong edges, which can occur when a vehicle is near an intersection or an overpass, for example. Bearing also helps the solver determine the side of the street that the point is on.

Learn more about bearing and bearing tolerance

BearingTolnumber (range: 0 - 180)default:30nullable

The bearing tolerance value creates a range of acceptable bearing values when locating moving points on an edge using the Bearing attribute.

If the Bearing attribute value is within the range of acceptable values that are generated from the bearing tolerance on an edge, the point can be added as a network location there; otherwise, the closest point on the next-nearest edge is evaluated. The units are in degrees. A value of 30 means that when Network Analyst attempts to add a network location on an edge, a range of acceptable bearing values is generated 15 degrees to either side of the edge (left and right) and in both digitized directions of the edge.

Learn more about bearing and bearing tolerance

NavLatencynumber (non-negative)nullable

Indicates how much cost is expected to elapse from the moment GPS information is sent from a moving vehicle to a server and the moment the processed route is received by the vehicle's navigation device.

This attribute is only used in the solve process if the Bearing and BearingTol fields also have values; however, providing a NavLatency attribute value is optional, even when values are present in the Bearing and BearingTol.

Example

The example shows how to specify some attributes for the demand_points.

{
  "features": [
    {
      "geometry": {
        "y": 51.5254,
        "x": -0.1891
      },
      "attributes": {
        "Name": "Customer 1",
        "ID": "C00001",
        "Weight": 10,
        "CurbApproach": 0
      }
    },
    {
      "geometry": {
        "y": 51.5353,
        "x": -0.1744
      },
      "attributes": {
Optional parameters
travel_mode
travel_mode
optional
Choose the mode of transportation for the analysis.

Travel modes are managed and configured in ArcGIS Online by the administrator of your organization to better reflect the organization's workflows.

In the example below, the following is a string representing the Walking Time travel mode as returned by the GetTravelModes tool:

"{\"attributeParameterValues\": [{\"attributeName\": \"Avoid Private Roads\", \"parameterName\": \"Restriction Usage\", \"value\": \"AVOID_MEDIUM\"}, {\"attributeName\": \"Walking\", \"parameterName\": \"Restriction Usage\", \"value\": \"PROHIBITED\"}, {\"attributeName\": \"Preferred for Pedestrians\", \"parameterName\": \"Restriction Usage\", \"value\": \"PREFER_LOW\"}, {\"attributeName\": \"WalkTime\", \"parameterName\": \"Walking Speed (km/h)\", \"value\": 5}, {\"attributeName\": \"Avoid Roads Unsuitable for Pedestrians\", \"parameterName\": \"Restriction Usage\", \"value\": \"AVOID_HIGH\"}], \"description\": \"Follows paths and roads that allow pedestrian traffic and finds solutions that optimize travel time. The walking speed is set to 5 kilometers per hour.\", \"distanceAttributeName\": \"Kilometers\", \"id\": \"caFAgoThrvUpkFBW\", \"impedanceAttributeName\": \"WalkTime\", \"name\": \"Walking Time\", \"restrictionAttributeNames\": [\"Avoid Private Roads\", \"Avoid Roads Unsuitable for Pedestrians\", \"Preferred for Pedestrians\", \"Walking\"], \"simplificationTolerance\": 2, \"simplificationToleranceUnits\": \"esriMeters\", \"timeAttributeName\": \"WalkTime\", \"type\": \"WALK\", \"useHierarchy\": false, \"uturnAtJunctions\": \"esriNFSBAllowBacktrack\"}"
The value above should be converted to a valid JSON object and passed as the value for the travel_mode parameter as shown below.

{
  "attributeParameterValues": [
    {
      "attributeName": "Avoid Private Roads",
      "parameterName": "Restriction Usage",
      "value": "AVOID_MEDIUM"
    },
    {
      "attributeName": "Walking",
      "parameterName": "Restriction Usage",
      "value": "PROHIBITED"
    },
    {
      "attributeName": "Preferred for Pedestrians",
      "parameterName": "Restriction Usage",
      "value": "PREFER_LOW"
    },
    {
      "attributeName": "WalkTime",
      "parameterName": "Walking Speed (km/h)",
locate_Settings
locate_settings
optional
Specify settings that affect how inputs are located, such as the maximum search distance to use when locating the inputs on the network or the network sources being used for locating.

The default value is as follows:

{
  "default": {
    "tolerance": 20000,
    "toleranceUnits": "esriMeters",
    "allowAutoRelocate": true,
    "sources": [
      {
        "name": "main.Routing_Streets"
      }
    ]
  }
}
Note
Currently, you can't specify different source names for the sources array. Also, allowAutoRelocate is always set to true since the request does not support location fields.

You can specify locate settings and can override locate settings for individual features such as, facilities, demand_points, point_barriers, line_barriers, and polygon_barriers through locator JSON object.

Show examples
measurement_units
string
optional
Default value: Minutes
Allowed values: Meters, Kilometers, Feet, Yards, NauticalMiles, Seconds, Minutes, Hours, Days

Specify the units that will be used to measure the travel times or travel distances between demand points and facilities.

The request finds the best facilities based on those that can reach, or be reached by, the most amount of weighted demand with the least amount travel. The output allocation lines report travel distance or travel time in different units, including the units you specify for this parameter.

Note
The units of the default_measurement_cutoff parameter and the attribute values of Cutoff on the facilities and demand_points parameter are same as the units of the measurement_units.

analysis_region
string
optional
Allowed values: Europe, Japan, Korea, MiddleEastAndAfrica, NorthAmerica, SouthAmerica, SouthAsia, Thailand

Specify the region in which to perform the analysis. If a value is not specified for this parameter, the request will automatically calculate the region name based on the location of the input points. Setting the name of the region is recommended to speed up the analysis.

The data coverage page lists the countries that are grouped into each of these regions.

problem_type
string
optional
Default value: Minimize Impedance
Allowed values: Maximize Attendance, Maximize Capacitated Coverage, Maximize Coverage, Maximize Market Share, Minimize Facilities, Minimize Impedance, Target Market Share

Specify the objective of the location-allocation analysis, which can be one of the following options:

Maximize Attendance—Facilities are chosen such that as much demand weight as possible is allocated to facilities while assuming the demand weight decreases in relation to the distance between the facility and the demand point.

Maximize Attendance problem type
Maximize Attendance chooses facilities such that as much demand weight as possible is allocated to facilities while assuming the demand weight decreases with distance. The demand points, represented by pie charts in this graphic, show how much of their total demand is captured by the facility.

Expand to learn more
Maximize Coverage—Facilities are located such that as much demand as possible is allocated to solution facilities within the impedance cutoff.

Maximize Coverage problem type
Maximize Coverage chooses facilities such that as much demand as possible is covered by the impedance cutoff of facilities. In this graphic, the task was directed to choose three facilities.

Expand to learn more
Maximize Capacitated Coverage—Facilities are located such that all or the greatest amount of demand can be served without exceeding the capacity of any facility.

Maximize Capacitated Coverage problem type
Maximize Capacitated Coverage chooses facilities such that all or the greatest amount of demand can be served without exceeding the capacity of any facility. In this graphic, each facility has a capacity of one, and the task was directed to choose three facilities. Although the demand point on the bottom of the map is within the impedance cutoff of a facility, it's not allocated, because doing so would surpass a facility's capacity.

Expand to learn more
Maximize Market Share—A specific number of facilities are chosen such that the allocated demand is maximized in the presence of competitors. The goal is to capture as much of the total market share as possible with a given number of facilities, which you specify. The total market share is the sum of all demand weight for valid demand points.

Maximize Market Share problem type
Maximize Market Share chooses facilities such that the largest amount of allocated demand is captured in the presence of competitors. You specify the number of facilities you want it to choose.

Expand to learn more
Minimize Facilities—Facilities are chosen such that as much weighted demand as possible is allocated to solution facilities within the travel time or distance cutoff; additionally, the number of facilities required to cover demand is minimized.

Minimize Facilities problem type
Minimize Facilities chooses facilities such that as many demand points as possible are within the impedance cutoff of facilities. Additionally, the number of facilities required to cover all demand points is minimized. In this graphic, the task was able to cover all demand points with only two facilities.

Expand to learn more
Minimize Impedance—This is also known as the P-Median problem type. Facilities are located such that the sum of all weighted travel time or distance between demand points and solution facilities is minimized. (Weighted travel is the amount of demand allocated to a facility multiplied by the travel distance or time to the facility.)

Minimize Impedance problem type
Minimize Impedance chooses facilities such that the sum of weighted impedances (demand allocated to a facility multiplied by the impedance to the facility) is minimized.

Expand to learn more
Target Market Share—Target Market Share chooses the minimum number of facilities necessary to capture a specific percentage of the total market share in the presence of competitors. The total market share is the sum of all demand weight for valid demand points. You set the percent of the market share you want to reach and let the solver choose the fewest number of facilities necessary to meet that threshold.

Target Market Share problem type
Target Market Share works in the presence of competitors and tries to choose the fewest facilities necessary to capture the market share that you specify.

Expand to learn more
number_of_facilities_to_find
integer
optional
Default value: 1
Specify the number of facilities the task should choose.

The facilities with a FacilityType attribute value of 1 (Required) are always chosen first. Any excess facilities to choose are picked from candidate facilities, which have a FacilityType attribute value of 2. If the number of facilities to find is less than the number of required facilities, an error occurs.

The number_of_facilities_to_find parameter is ignored for the Minimize Facilities and Target Market Share problem types since the task determines the minimum number of facilities needed to meet the objectives.

default_measurement_cutoff
number
optional
Default value: None
Specify the maximum travel time or distance allowed between a demand point and the facility to which it is allocated. If a demand point is outside the cutoff of a facility, it cannot be allocated to that facility.

The default value of None means travel isn't limited. The units for this parameter are the same as those specified by the measurement_units parameter. The travel time or distance is measured by the shortest path along roads. This property might be used to model the maximum distance that people are willing to travel to visit stores or the maximum time that is permitted for a fire department to reach anyone in the community.

Note that demand points have TimeCutoff and DistanceCutoff attributes, which, if set accordingly, override the default_measurement_cutoff parameter. You may find that people in rural areas are willing to travel up to 10 miles to reach a facility while urbanites are only willing to travel up to two miles. Assuming measurement_units is set to miles, you can model this behavior by setting the default_measurement_cutoff to 10 and the DistanceCutoff attribute value of the demand points in urban areas to 2.

default_capacity
number
optional
Default value: 1
This parameter is specific to the Maximize Capacitated Coverage problem type and is ignored for all other problem types. It is the default capacity assigned to all facilities in the analysis. You can override the default capacity for a facility by specifying a value in the facility's Capacity attribute.

target_market_share
number
optional
Default value: 10
This parameter is specific to the Target Market Share problem type and is ignored for all other problem types. It is the percentage of the total demand weight that you want the chosen and required facilities to capture. The task chooses the minimum number of facilities needed to capture the target market share specified here.

measurement_transformation_model
string
optional
Default value: Linear
Allowed values: Linear, Power, Exponential

Specify the equation for transforming the network cost between facilities and demand points. This parameter, coupled with impedance parameter, specifies how severely the network impedance between facilities and demand points influences the task's choice of facilities.

In the following list of transformation options, d refers to demand points and f, facilities. "Impedance" refers to the shortest travel distance or time between two locations. So impedance df is the shortest path (time or distance) between demand point d and facility f, and costdf is the transformed travel time or distance between the facility and demand point. Lambda (λ) denotes the impedance parameter. The measurement_units setting determines whether travel time or distance is analyzed.

Linear

costdf = λ * impedancedf

The transformed travel time or distance between the facility and the demand point is the same as the time or distance of the shortest path between the two locations. With this option, the impedance parameter (λ) is always set to one.

Power

costdf = impedancedfλ

The transformed travel time or distance between the facility and the demand point is equal to the time or distance of the shortest path raised to the power specified by the impedance parameter (λ). Use the Power option with a positive impedance parameter to specify higher weight to nearby facilities.

Exponential

costdf = e(λ * impedance)

The transformed travel time or distance between the facility and the demand point is equal to the mathematical constant e raised to the power specified by the shortest-path network impedance multiplied with the impedance parameter (λ). Use the Exponential option with a positive impedance parameter to specify a very high weight to nearby facilities.

Example of measurement transformation model
measurement_transformation_factor
number
optional
Default value: 1
Provides an impedance parameter value (λ) to the equation specified in the measurement_transformation_model parameter. The parameter value is ignored when the impedance transformation is linear. For power and exponential impedance transformations, the value should be nonzero.

travel_direction
string
optional
Default value: Facility to Demand
Allowed values: Facility to Demand, Demand to Facility

Specify whether to measure travel times or distances from facilities to demand points or from demand points to facilities.

Facility to Demand—Direction of travel is from facilities to demand points.
Demand to Facility—Direction of travel is from demand points to facilities.
Travel times and distances may change based on direction of travel. If you're going from point A to point B, you may encounter less traffic or have a shorter path, due to one-way streets and turn restrictions, than if you were traveling in the opposite direction. For instance, going from point A to point B may only take 10 minutes, but going the other direction may take 15 minutes. These differing measurements may affect whether demand points can be assigned to certain facilities because of cutoffs or, in problem types in which demand is apportioned, affect how much demand is captured.

Fire departments commonly measure from facilities to demand points since they are concerned with the time it takes to travel from the fire station to the location of the emergency. A retail store is more concerned with the time it takes shoppers to reach the store; therefore, stores commonly measure from demand points to facilities.

The travel_direction parameter also determines the meaning of any start time that is provided. See the time_of_day parameter for more information.

time_of_day
datetime
optional
Specify the time at which travel begins, or departs, from the starting locations.

Specifying a time of day results in more accurate estimations of travel times because the travel times account for the traffic conditions that are applicable for that date and time.

To use traffic in the analysis, choose a time-based unit for impedance and assign a value to time_Of_day.

The time_Of_day value represents the target start time from facilities or demand points, depending on the travel_direction. The time is specified as Unix time (milliseconds since midnight, January 1, 1970).

If a time of day is not passed in, the request uses static road speeds based on average historical speeds or posted speed limits. It uses posted speeds in areas where historical traffic information isn't available.

Note
Traffic is supported only with the driving time impedance or travel mode.

If the time_Of_day specified is within 4 hours of the current time, live traffic will be used where available. Live traffic retrieves speeds based on phone probe records, sensors, and other data sources and reflects the current travel speeds and predicts speeds for the near future. If the time_Of_day specified is earlier than 4 hours or later than 4 hours from the current time, or the road does not have live traffic, typical traffic speeds will be used. Typical speeds are based on historical traffic patterns. The travel time data is aggregated in 15 minute intervals per day of week based on multiple years worth of data. So a road may have a different travel time at Monday at 8 am, Monday at 8:15 am, or Tuesday at 8 am. Since the variance is just at the day of week and time of day, the travel time is the same on a road for any Monday at 8 am, regardless of the month or year.

If your goal is to model typical travel conditions and avoid large variances from the average due to live traffic, it is recommended to use a date from the past to ensure it doesn't coincide with the 4 hour window from the current time. As an extreme example, you can even use dates from 1990.

The Data Coverage page shows the countries Esri currently provides traffic data for.

The request support two kinds of traffic: live and typical.

Typical traffic
To ensure the task uses typical traffic in locations where it is available, choose a time and day of the week, and then convert the day of the week to one of the following dates from 1990:

Monday—1/1/1990
Tuesday—1/2/1990
Wednesday—1/3/1990
Thursday—1/4/1990
Friday—1/5/1990
Saturday—1/6/1990
Sunday—1/7/1990
Set the time and date as Unix time in milliseconds. For example, to solve for 1:03 p.m. on Thursdays, set the time and date to 1:03 p.m., January 4, 1990; and convert to milliseconds (631458180000). Although the dates representing days of the week are from 1990, typical traffic is calculated from recent traffic trends—usually over the last two years worth of data.

Live traffic
To use live traffic when and where it is available, choose a time and date and convert to Unix time.

Note
You need to have a Live Traffic extension and configure your Street Map Premium network dataset to access live traffic.

Esri saves live traffic data for 4 hours and references predictive data extending 4 hours into the future. If the time and date you specify for this parameter is outside the 8-hour time window, or the travel time in the analysis continues past the predictive data window, the task falls back to typical traffic speeds.

Show examples
time_zone_for_time_of_day
string
optional
Default value: Geographically Local
Allowed values: Geographically Local, UTC

Specify the time zone or zones of the time_Of_day parameter.

Geographically Local—The time_Of_day value refers to the time zone or zones in which the input points are located. This option causes the analysis to have rolling start times across time zones.

Illustration of setting the value to Geographically Local
UTC—The time_Of_day value refers to coordinated universal time (UTC). The start times for all points are simultaneous, regardless of time zones.

Illustration of setting the value to UTC
uturn_at_junctions
string
optional
Default value: Allowed only at Intersections and Dead Ends
Allowed values: Allowed, Allowed only at Intersections and Dead Ends, Allowed only at Dead Ends, Not Allowed

Specify whether to restrict or permit the route from making U-turns at junctions.

Attention
The value of this parameter, regardless of whether you rely on the default or explicitly set a value, is overridden when travel_mode is set to any value other than Custom.

To understand the available parameter values, a junction is a point where only two streets intersect each other. If three or more streets intersect at a point, it is called as an intersection. A cul-de-sac is a dead-end.

Expand to learn more about the available parameter values
point_barriers
feature
required
Specify one or more points that will act as temporary restrictions or represent additional time or distance that may be required to travel on the underlying streets. For example, a point barrier can be used to represent a fallen tree along a street or a time delay spent at a railroad crossing.

When specifying point barriers, you can set properties for each, such as its name or barrier type.

Show attributes for point_barriers
Show example
line_barriers
feature
required
Specify one or more lines that prohibit travel anywhere the lines intersect the streets. For example, a parade or protest that blocks traffic across several street segments can be modeled with a line barrier. A line barrier can also quickly fence off several roads from being traversed, thereby channeling possible routes away from undesirable parts of the street network.

Two maps demonstrate how a line barrier affects finding a route between two stops.
The first map displays the shortest path between two stops. The second map shows the shortest path when several streets are blocked by a polyline barrier.

When specifying line barriers, you can set the name of each barrier using the following attribute:

Show attributes for line_barriers
Show example
polygon_barriers
feature
required
Specify polygons that either completely restrict travel or proportionately scale the time or distance required to travel on the streets intersected by the polygons.

When specifying polygon barriers, you can set properties for each, such as its name or barrier type, using the following attributes:

Show attributes for polygon_barriers
Show example
use_hierarchy
boolean
optional
Default value: true
Specify whether hierarchy will be used when finding the shortest paths.

Attention
The value of this parameter, regardless of whether you rely on the default or explicitly set a value, is overridden when travel_mode is set to any value other than Custom.

true—Use hierarchy when travelling between stops. When hierarchy is used, the request prefers higher-order streets (such as freeways) to lower-order streets (such as local roads) and can be used to simulate the driver preference of traveling on freeways instead of local roads even if that means a longer trip. This is especially true when finding routes to faraway locations, because drivers on long-distance trips tend to prefer traveling on freeways where stops, intersections, and turns can be avoided. Using hierarchy is computationally faster, especially for long-distance routes, since the request can determine the best route from a relatively smaller subset of streets.
false—Do not use hierarchy when travelling between stops. When hierarchy is not used, the request considers all the streets and doesn't prefer higher-order streets when finding the route. This is often used when finding short-distance routes within a city.
Note
The request automatically reverts to using hierarchy if the straight-line distance between the stops is greater than 50 miles (80.46 kilometers), even if you have specified to find the route without using hierarchy.

restrictions
[string]
optional
Specify whether the restrictions will be honored by the request.

A restriction represents a driving preference or requirement. In most cases, restrictions cause roads or pathways to be prohibited, but they can also cause them to be avoided or preferred. For instance, using the Avoid Toll Roads restriction will result in a route that will include toll roads only when it is required to travel on toll roads to visit a stop. Use Height Restriction to route around clearances that are lower than the height of the vehicle. If the vehicle is carrying corrosive materials, you can use the Any Hazmat Prohibited restriction to prevent hauling the materials along roads where it is marked as illegal to do so.

Attention
The value of this parameter, regardless of whether you rely on the default or explicitly set a value, is overridden when travel_mode is set to any value other than Custom.

Note
Some restrictions are supported only in certain countries. If you specify restriction names that are not available in the country where the input points are located, the request ignores the invalid restrictions and returns warning messages indicating the names for the restrictions that were not considered when performing the analysis.

Note
You may need to specify an additional value, the restriction attribute parameter, for a restriction to get the intended results. This value must be associated with the restriction name and a restriction parameter using attribute_parameter_values .

The parameter value is specified as a comma-separated list of restriction names. A value of null indicates that no restrictions will be used when finding the best route, but only when travel_mode is set to Custom.

Example for restrictions
restrictions=[Driving a Truck, Height Restriction, Length Restriction]
Expand to see the restriction names supported by the request
attribute_parameter_values
table
optional
Specify additional values required by an attribute or restriction, such as to specify whether the restriction prohibits, avoids, or prefers travel on restricted roads. If the restriction is meant to avoid or prefer roads, you can further specify the degree to which they are avoided or preferred using this parameter. For example, you can choose to never use toll roads, avoid them as much as possible, or prefer them.

Attention
The value of this parameter, regardless of whether you rely on the default or explicitly set a value, is overridden when travel_mode is set to any value other than Custom.


More info





See the default Restriction Usage values for the restrictions
Syntax and code sample for attribute parameter values
SyntaxExample
{
  "features": [
    {
      "attributes": {
        "<field1>": "<value11>",
        "<field2>": "<value12>",
        "<field3>": "<value13>"
      }
    },
    {
      "attributes": {
        "<field1>": "<value21>",
        "<field2>": "<value22>",
        "<field3>": "<value13>"
      }
    }
  ]
}
allocation_line_shape
string
optional
Default value: Straight Line
Allowed values: Straight Line, None

Specify the type of line features that are output by the request. The parameter accepts the following values:

Straight Line—Straight lines between solution facilities and the demand points allocated to them are returned. Drawing straight lines on a map helps you visualize how demand is allocated.
None—A table containing data about the shortest paths between solution facilities and the demand points allocated to them is returned; lines are not returned.
Regardless of the value you choose for the allocation_line_shape parameter, the shortest route is determined by minimizing the travel time or the travel distance, not using the straight-line distance between incidents and facilities. That is, this parameter only changes the output line shapes; it doesn't change the measurement method.

time_impedance
string
optional
Allowed values: Minutes, TravelTime, WalkTime, TruckMinutes, TruckTravelTime

Specify time-based impedance.

Note
If the impedance for the travel mode, as specified using the impedance parameter, is time based, the values for the time_impedance and impedance parameters must be identical. Otherwise, the request will return an error.

Attention
The value of this parameter, regardless of whether you rely on the default or explicitly set a value, is overridden when travel_mode is set to any value other than Custom.

distance_impedance
string
optional
Default value: Miles
Allowed values: Miles, Kilometers

Specify distance-based impedance.

The value represents the travel distance along road segments or on other parts of the transportation network.

Miles—Length measurements along roads are stored in miles and can be used for performing analysis based on shortest distance.
Kilometers —Length measurements along roads are stored in kilometers and can be used for performing analysis based on shortest distance.
Note
If the impedance for the travel mode, as specified using the impedance parameter, is distance based, the values for the distance_impedance and impedance parameters must be identical. Otherwise, the request will return an error.

Attention
The value of this parameter, regardless of whether you rely on the default or explicitly set a value, is overridden when travel_mode is set to any value other than Custom.

impedance
string
optional
Allowed values: TravelTime, Minutes, TruckTravelTime, TruckMinutes, WalkTime, Miles, Kilometers

Specify the impedance.

Attention
The value of this parameter, regardless of whether you rely on the default or explicitly set a value, is overridden when travel_mode is set to any value other than Custom.

Impedance is a value that quantifies travel along the transportation network. Travel distance is an example of impedance; it quantifies the length of walkways and road segments. Similarly, drive time—the typical time it takes to drive a car along a road segment—is an example of impedance. Drive times may vary by type of vehicle—for instance, the time it takes for a truck to travel along a path tends to be longer than a car—so there can be many impedance values representing travel times for different vehicle types. Impedance values may also vary with time; live and typical traffic reference dynamic impedance values. Each walkway and road segment stores at least one impedance value. When performing a network analysis, the impedance values are used to calculate the best results, such as finding the shortest route—the route that minimizes impedance—between two points.

The parameter can be specified using the following values:

TravelTime—Historical and live traffic data is used. This option is good for modeling the time it takes automobiles to travel along roads at a specific time of day using live traffic speed data where available. When using TravelTime, you can optionally set the TravelTime::Vehicle Maximum Speed (km/h) attribute parameter to specify the physical limitation of the speed the vehicle is capable of traveling.
Minutes—Live traffic data is not used, but historical average speeds for automobiles data is used.
TruckTravelTime—Historical and live traffic data is used, but the speed is capped at the posted truck speed limit. This is good for modeling the time it takes for the trucks to travel along roads at a specific time. When using TruckTravelTime, you can optionally set the TruckTravelTime::Vehicle Maximum Speed (km/h) attribute parameter to specify the physical limitation of the speed the truck is capable of traveling.
TruckMinutes—Live traffic data is not used, but the smaller of the historical average speeds for automobiles and the posted speed limits for trucks are used.
WalkTime—The default is a speed of 5 km/hr on all roads and paths, but this can be configured through the WalkTime::Walking Speed (km/h) attribute parameter.
Miles—Length measurements along roads are stored in miles and can be used for performing analysis based on shortest distance.
Kilometers—Length measurements along roads are stored in kilometers and can be used for performing analysis based on shortest distance.
If you choose a time-based impedance, such as TravelTime, TruckTravelTime, Minutes, TruckMinutes, or WalkTime, the measurement_units parameter must be set to a time-based value; if you choose a distance-based impedance such as Miles, Kilometers, the measurement_units must be distance-based.

Note
Drive Time, Truck Time, Walk Time, and Travel Distance impedance values are no longer supported and will be removed in a future release.

save_output_network_analysis_layer
boolean
optional
Default value: false
Specify whether the request will save the analysis settings as a network analysis layer package file.

You cannot work directly with this file even when you open the file in an ArcGIS Desktop application such as ArcGIS Pro. It is meant to be sent to Esri Technical Support to diagnose the quality of results returned from the request.

true—The network analysis layer package file will be saved. The file can be downloaded from the URL provided as part of the output_network_analysis_layer_package parameter.

false—The network analysis layer package file will not be saved.

output_format
string
optional
Default value: Feature Set
Allowed values: Feature Set, JSON File, GeoJSON File

Specify the format in which the output features will be returned.

Feature Set—The output features will be returned as feature classes and tables.

JSON File—The output features will be returned as a compressed file containing the JSON representation of the outputs. When this option is specified, the output is a single file (with a .zip extension) that contains one or more JSON files (with a .json extension) for each of the outputs created by the request.

GeoJSON File—The output features will be returned as a compressed file containing the GeoJSON representation of the outputs. When this option is specified, the output is a single file (with a .zip extension) that contains one or more GeoJSON files (with a .geojson extension) for each of the outputs created by the request.

Note
Specifying file based output format, such as a JSON File, is useful when you are calling the request using the REST endpoint of the request. In such cases, returning all the outputs as a single file allows you to download large results that can be generated by the request. For example, if you are working with the GenerateOriginDestinationCostMatrix request and you generate a travel matrix with 1,000,000 records, returning such a large output as a Feature Set can cause the request to fail since the it will try to send the entire output in a single attempt. With a file-based output, the request sends the output in multiple chunks, reducing the possibility of timeouts.

context
context_object
required
This parameter contains additional settings that affect task operation, for example, the spatial reference of the output features.

overrides
Note
This parameter is for internal use only.

ignore_invalid_locations
boolean
optional
Default value: true
Specify whether invalid input locations will be ignored.

true—Network locations that are unlocated will be ignored and the analysis will run using valid network locations only. The analysis will also continue if locations are on non-traversable elements or have other errors. This is useful if you know the network locations are not all correct, but you want to run the analysis with the network locations that are valid.
false—Invalid locations will not be ignored. Any invalid point will cause the request to return a failure.
Response objects
Upon successful completion, the request returns the chosen facilities, participating demand points, connecting lines between demand points and the facilities they were assigned to, and the status of whether the analysis was successful.

output_facilities
feature
optional
Provides access to the chosen, required, and competitor facilities, as well as any candidate facilities that were not chosen.

Show attributes for output facilities
output_demand_points
feature
optional
type:feature

Provides access to the demand points that participated in the analysis: those that were and were not allocated to facilities.

Show attributes for output demand points
output_allocation_lines
feature
optional
Provides access to the lines that connect demand points to the facilities to which they are allocated. Such lines are referenced in the documentation as allocation lines.

Show attributes for output allocation lines
output_network_analysis_layer_package
feature
optional
Provides access to the network analysis layer package file that stores the analysis settings and the inputs and outputs used for the analysis.

The parameter value is populated only when the save_output_network_analysis_layer parameter is set to True.

usage_cost
usage_cost_object
optional
This parameter returns the credits used by the analysis.

Example

The following shows an example of the usage_cost parameter in which the analysis generated 9 billable objects (represented by numObjects ) and 4.5 credits were used by the analysis.

{
  "paramName": "Usage_Cost",
  "dataType": "GPString",
  "value": {
    "numObjects": 9,
    "credits": 4.5
  }
}
solve_succeeded
boolean
optional
Determine if the request was able to complete successfully. The error messages for the failure can be obtained by making a request to get the status of the job.

Example

The following shows an example of the solve_succeeded parameter

{
  "paramName": "solve_succeeded",
  "dataType": "GPBoolean",
  "value": true
}
output_result_file
File
optional
Use this parameter to access the results from the analysis as a .zip file containing one or more files for each output. The format of the individual file is specified by the Output Format parameter. The parameter value is not populated when the output_format parameter is set to Feature Set.

Examples
Mapping and location services guide
To build client application see API reference examples

Warning
If you copy and paste the request URL from the examples into a web browser, you need to replace <yourToken> with a valid token. See Security and authentication to learn more.

Choose the best store location
In this example, you will find the one store location that provides the best access to customers.

Before performing the analysis, you will need to find locations that could accommodate your store. This may include surveying the real estate market to find commercial properties that are the right size and have the right price. The candidate facilities are specified with the facilities facilities parameter. Households are added as demand_points and weighted by the number of people living there. They represent potential customers.

The facilities and demand points are in the default spatial reference, WGS84, so the spatialReference property is not required.

1. Submit job2. Check job status3. Get output_facilities4. Get output_demand_points
The first request submits a job and returns the job id.

Request

POST https://logistics.arcgis.com/arcgis/rest/services/World/LocationAllocation/GPServer/SolveLocationAllocation/submitJob? HTTP/1.1
Content-Type: application/x-www-form-urlencoded

f=json
&token=<ACCESS_TOKEN>
&facilities={
    "features": [
        {
            "attributes": {
                "OBJECTID": 1,
                "Name": "Facility A",
                "FacilityType": 0,
                "CurbApproach": 0
            },
            "geometry": {
                "x": -58.557329417999938,
                "y": -34.587693706999971
            }
        },
        {
Response (JSON)

{
  "jobId": "jb937dc0c7e324e68b95923c8fd9c8a1b",
  "jobStatus": "esriJobSubmitted"
}
Service limits
Note
The service works in all the supported countries as listed in the data coverage page. One or more countries are grouped together to form an analysis region. The service can determine the best region to use based on the location of the inputs; otherwise, you can use a parameter to specify a region. The service does not support requests that span more than one region. Consequently, the service will only generate results for inputs that fall within one region.

The table below lists the limits that apply to the request.

Limit Description	Limit Value
Maximum number of facilities:

1,000

Maximum number of facilities to find:

100

Maximum number of demand points:

10,000

Maximum number of (point) barriers:

250

Maximum number of street features intersected by polyline barriers:

500

Maximum number of street features intersected by polygon barriers:

2,000

Force hierarchy beyond a straight-line distance of:

(If the straight-line distance between any facility and demand point is greater than the limit shown here, the analysis uses hierarchy, even if useHierarchy is set to false.)

50 miles (80.46 kilometers)

Maximum execution time

(If the limit is exceeded, the request stops processing and the job status is set to esriJobFailed)

4 hours