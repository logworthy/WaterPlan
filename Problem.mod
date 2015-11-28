# C:\NotBackedUp\gusek_0-2-18\gusek\glpsol.exe --model "\\svrau100qsm01.oceania.corp.anz.com\PFSMB PBWMg\Deposits\Cash&TDs\Pricing Analysis\Pricing Tool\Test & Learn\2015_03 State and FUM\Analysis\LPs\vanilla.mod" -d "\\svrau100qsm01.oceania.corp.anz.com\PFSMB PBWMg\Deposits\Cash&TDs\Pricing Analysis\Pricing Tool\Test & Learn\2015_03 State and FUM\Analysis\LPs\sft_prcg_3dim.dat"

#F:\NotBackedUp\gusek_0-2-18\gusek\glpsol.exe  --cover --clique --gomory --mir -m "Problem.mod" -d "Problem.dat" -o /dev/stdout  

set CITIES;
set PLANTS;
set TIERS;

# max plant size
param capacity{p in PLANTS}; # max capacity of a plant m3

# max city demand
param demand{c in CITIES};

# operating costs
param op_fixed_cost := 90000; # monthly fixed operating cost
param op_var_cost := 3.6; # monthly fixed cost / m3

# construction costs
# construction costs benefit from economies of scale
# 7 tiers but scaling within tiers assumed to be linear
param build_base_cost{t in TIERS};
param build_base_capacity{t in TIERS};
param build_max_capacity{t in TIERS};
param build_unit_cost{t in TIERS};

# distribution costs
param plant2city_cost{p in PLANTS, c in CITIES}; # costs to move between plants -> cities
param city2city_cost{a in CITIES, b in CITIES}; # costs to move cities -> cities

# variables
var operating_cost{p in PLANTS} >= 0;
var build_cost{p in PLANTS} >= 0;
var build_tier{p in PLANTS, t in TIERS} binary; # does plant p belong to tier t?
var production{p in PLANTS} >= 0; # how much does plant p produce?
var consumption{c in CITIES} >= 0; # how much does city c consume?
var price >= 0; # how much to sell water for
var plant2city{p in PLANTS, c in CITIES} >= 0; # how much to deliver from p to c
var city2city{a in CITIES, b in CITIES} >= 0; # how much to deliver from a to b
var delivery_costs;

# objective is to maximize water consumption
maximize total_consumption: sum{c in CITIES} consumption[c]; 

# constraints

# a plant can only belong to one tier
one_tier{p in PLANTS}:
	sum{t in TIERS} build_tier[p,t] <= 1;

# a plant can't produce more than its tier
production_tier_max{p in PLANTS}: 
	production[p] <= sum{t in TIERS} (build_max_capacity[t] * build_tier[p,t]);

# a plant can't produce less than its tier
production_tier_base{p in PLANTS}: 
	production[p] >= sum{t in TIERS} build_base_capacity[t] * build_tier[p,t];
	
# a plant can't produce more than its max capacity
production_capacity{p in PLANTS}:
	production[p] <= capacity[p];

# a plant can't deliver more than it produces
plant_delivery_limit{p in PLANTS}:
	sum{c in CITIES} plant2city[p,c] <= production[p];
	
# a city can't deliver more than it receives and consumes
city_delivery_limt{c in CITIES}:
	sum{a in CITIES} city2city[c,a] <= 
	sum{p in PLANTS} plant2city[p,c] +
	sum{b in CITIES} city2city[b,c] -
	consumption[c];
	
# consumption of a city cannot exceed inflow
city_consumption_limt{c in CITIES}:
	consumption[c] <= 
	sum{p in PLANTS} plant2city[p,c] +
	sum{b in CITIES} city2city[b,c] -
	sum{a in CITIES} city2city[c,a];
	
# consumption of a city cannot exceed demand
city_demand_limit{c in CITIES}:
	consumption[c] <= demand[c];
	
# costs
plant_operating_cost{p in PLANTS}:
	operating_cost[p] = op_fixed_cost + op_var_cost * production[p];
	
delivery_cost_constraint:
	delivery_costs = 
	sum{a in CITIES, b in CITIES} (city2city[a,b] * city2city_cost[a,b])+
	sum{p in PLANTS, c in CITIES} (plant2city[p,c] * plant2city_cost[p,c]);
	
#breakeven:
#	delivery_costs + sum{p in PLANTS} operating_cost[p] <= 
#	price[i] * sum{c in CITIES} consumption[c];
end;







