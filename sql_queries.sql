#sql queries for march madness metrics------------------

#popular locations for SRS_f 
select avg(SRS_f) as avg_srs_f,
avg(SRS_u) as avg_srs_u, Location,
count(Location) as cnt_location  
from funX.tropics
group by 3 
order by 3  

#do higher seeds' travel vary from year to year? 
select avg(distance_f) as avg_dist_f,
avg(distance_u) as avg_dist_u, 
year
from funX.tropics
where year !=2018 
group by 3 
order by 1 


#which team played the most NCAA games 
select count(team_fav) as games_f,
team_fav
from funX.tropics
where year !=2018 
group by 2 
order by 1 

#which high seeds play the highest SRS?
select team_fav,
avg(SRS_u) as avg_srs_opp,
count(team_fav) as count_fav
from funX.tropics 
where year !=2018 and seed2>4 #opponent higher than 4 seed
group by 1
order by 2 

#which low seeds play the highest SRS?

select team_under,
avg(SRS_f) as avg_srs_opp,
count(team_under) as count_fav
from funX.tropics 
where year !=2018 and seed1 #opponent higher than 4 seed
group by 1
order by 2  */

#13-16 seeds travel less than 1-4?
select team_fav,
team_under, distance_f,
distance_u, seed1, seed2,
Location 
from funX.tropics
where seed1 between 1 and 4 and year !=2018 
and seed2 between 13 and 16 
and distance_f>distance_u */

#do cinderella teams shoot above average threes per game? 
select team_fav,
team_under,
three_PA_u
from funX.tropics
where three_PA_u>(select avg(three_PA_u) from funX.tropics)
order by 3  

#attempt two 
select team_fav,
team_under,
three_PA_u,
win_home,
year 
from funX.tropics
where (three_PA_u<(select avg(three_PA_u) from funX.tropics)) and
(year !=2018) and (win_home=0) and (seed2 between 13 and 16) and
(seed 1 between 1 and 4) */ #over 4, under 4 

#seeds SRS rankings 
select seed1,
avg(SRS_f) as avg_srs_f
from funX.tropics
where seed1 !=16  and year !=2018 
group by seed1
