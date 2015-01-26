CREATE TEMPORARY TABLE IF NOT EXISTS X AS (select distinct MovieID from ratings);SELECT * from cast_info where role_id<=2 and movie_id in (select id from X);
SELECT person_id, count(*) as c from cast_info where role_id<=2 and movie_id in (select MovieID from X) group by person_id having c>10 ORDER BY count(*) DESC
