pilot(lamb).
pilot(besenyei).
pilot(chambliss).
pilot(maclean).
pilot(mangold).
pilot(jones).
pilot(bonhomme).

team(lamb,breitling).
team(besenyei,red_bull).
team(chambliss,red_bull).
team(maclean,mediterranean_racing_team).
team(mangold,cobra).
team(jones,matador).
team(bonhomme,matador).

plane(lamb,mx2).
plane(besenyei,edge540).
plane(chambliss,edge540).

plane(maclean,edge540).
plane(mangold,edge540).
plane(jones,edge540).
plane(bonhomme,edge540).

circuit(instanbul).
circuit(budapest).
circuit(porto).

won(jones,porto).
won(mangold,budapest).
won(mangold,instanbul).

gates(instanbul,9).
gates(budapest,6).
gates(porto,5).

wonTeam(Team,Circuit) :- circuit(Circuit),team(Pilot,Team),won(Pilot,Circuit).