% Test queries for Call of Duty Weapon Knowledgebase
% Run these queries to test the functionality of the knowledgebase

% Load the knowledgebase
:- consult('../src/weapons.pl').

% Basic query tests
test_gun_types :-
    write('Testing gun_type/2 predicate:'), nl,
    write('All assault rifles:'), nl,
    forall(gun_type(assault_rifle, Gun), (write('  '), write(Gun), nl)),
    nl.

test_games :-
    write('Testing gun_in_game/2 predicate:'), nl,
    write('All guns in Modern Warfare (2007):'), nl,
    forall(gun_in_game(modern_warfare, Gun), (write('  '), write(Gun), nl)),
    nl.

test_damage_levels :-
    write('Testing high damage guns:'), nl,
    forall(high_damage_guns(Gun), (write('  '), write(Gun), nl)),
    nl.

test_fire_rates :-
    write('Testing fast firing guns:'), nl,
    forall(fast_firing_guns(Gun), (write('  '), write(Gun), nl)),
    nl.

% Recursive query tests
test_direct_unlock :-
    write('Testing direct unlock progression:'), nl,
    write('What can M4A1 unlock?'), nl,
    forall(can_unlock(m4a1, NextGun), (write('  '), write(NextGun), nl)),
    nl.

test_recursive_unlock :-
    write('Testing recursive unlock progression:'), nl,
    write('What can M4A1 eventually unlock?'), nl,
    forall(can_unlock_eventually(m4a1, Gun), (write('  '), write(Gun), nl)),
    nl.

test_unlock_chains :-
    write('Testing unlock chains:'), nl,
    write('Complete unlock chain starting from M4A1:'), nl,
    unlock_chain(m4a1, Chain),
    write('  '), write(Chain), nl,
    nl.

test_progression_depth :-
    write('Testing progression depth:'), nl,
    write('Progression depths for various guns:'), nl,
    forall((gun(Gun, _, _, _, _, _), progression_depth(Gun, Depth)),
           (write('  '), write(Gun), write(': depth '), write(Depth), nl)),
    nl.

test_era_queries :-
    write('Testing era-based queries:'), nl,
    write('Modern era guns:'), nl,
    forall(modern_era_guns(Gun), (write('  '), write(Gun), nl)),
    nl,
    write('Classic era guns:'), nl,
    forall(classic_era_guns(Gun), (write('  '), write(Gun), nl)),
    nl,
    write('Black Ops series guns:'), nl,
    forall(black_ops_series_guns(Gun), (write('  '), write(Gun), nl)),
    nl.

test_utility_rules :-
    write('Testing utility rules:'), nl,
    write('All sniper rifles:'), nl,
    guns_by_type(sniper_rifle, SniperRifles),
    write('  '), write(SniperRifles), nl,
    nl.

test_longest_chains :-
    write('Testing longest unlock chains:'), nl,
    write('Longest chain from M1 Garand:'), nl,
    longest_chain_from(m1_garand, Chain),
    write('  '), write(Chain), nl,
    nl.

% Test new weapon types
test_new_weapon_types :-
    write('Testing new weapon types:'), nl,
    write('All pistols:'), nl,
    findall(Gun, pistol_guns(Gun), Pistols),
    length(Pistols, PistolCount),
    write('  Found '), write(PistolCount), write(' pistols'), nl,

    write('All shotguns:'), nl,
    findall(Gun, shotgun_guns(Gun), Shotguns),
    length(Shotguns, ShotgunCount),
    write('  Found '), write(ShotgunCount), write(' shotguns'), nl,

    write('All marksman rifles:'), nl,
    findall(Gun, marksman_rifle_guns(Gun), MarksmanRifles),
    length(MarksmanRifles, MRCount),
    write('  Found '), write(MRCount), write(' marksman rifles'), nl,

    write('All battle rifles:'), nl,
    findall(Gun, battle_rifle_guns(Gun), BattleRifles),
    length(BattleRifles, BRCount),
    write('  Found '), write(BRCount), write(' battle rifles'), nl,
    nl.

% Test weapon classifications
test_weapon_classifications :-
    write('Testing weapon classifications:'), nl,
    write('Primary weapons (sample):'), nl,
    findall(Gun, primary_weapon(Gun), PrimaryWeapons),
    length(PrimaryWeapons, PrimaryCount),
    write('  Found '), write(PrimaryCount), write(' primary weapons'), nl,

    write('Secondary weapons (sample):'), nl,
    findall(Gun, secondary_weapon(Gun), SecondaryWeapons),
    length(SecondaryWeapons, SecondaryCount),
    write('  Found '), write(SecondaryCount), write(' secondary weapons'), nl,

    write('Close quarters weapons:'), nl,
    findall(Gun, close_quarters_weapon(Gun), CQWeapons),
    length(CQWeapons, CQCount),
    write('  Found '), write(CQCount), write(' close quarters weapons'), nl,

    write('Long range weapons:'), nl,
    findall(Gun, long_range_weapon(Gun), LRWeapons),
    length(LRWeapons, LRCount),
    write('  Found '), write(LRCount), write(' long range weapons'), nl,
    nl.

% Test attachment system
test_attachment_system :-
    write('Testing attachment system:'), nl,
    write('All optic attachments:'), nl,
    findall(Att, attachment_type(optic, Att), Optics),
    write('  '), write(Optics), nl,

    write('Damage boosting attachments:'), nl,
    findall(Att, damage_boosting_attachments(Att), DamageAtts),
    write('  '), write(DamageAtts), nl,

    write('Stealth attachments:'), nl,
    findall(Att, stealth_attachments(Att), StealthAtts),
    write('  '), write(StealthAtts), nl,

    write('M4A1 compatible attachments:'), nl,
    findall(Att, compatible_attachments(m4a1, Att), M4Attachments),
    write('  '), write(M4Attachments), nl,
    nl.

% Test game mechanics
test_game_mechanics :-
    write('Testing game mechanics:'), nl,
    write('Progression mechanics:'), nl,
    findall(Mech, progression_mechanics(Mech), ProgMechanics),
    write('  '), write(ProgMechanics), nl,

    write('Modern mechanics:'), nl,
    findall(Mech, modern_mechanics(Mech), ModernMechs),
    length(ModernMechs, ModernCount),
    write('  Found '), write(ModernCount), write(' modern mechanics'), nl,

    write('Unlock mechanics:'), nl,
    findall(Mech, unlock_mechanics(Mech), UnlockMechs),
    write('  '), write(UnlockMechs), nl,
    nl.

% Test advanced combinations
test_advanced_combinations :-
    write('Testing advanced weapon combinations:'), nl,
    write('Well-supported weapons (3+ attachments):'), nl,
    findall(Gun, well_supported_weapons(Gun), WellSupported),
    length(WellSupported, WSCount),
    write('  Found '), write(WSCount), write(' well-supported weapons'), nl,

    write('Best close quarters weapons:'), nl,
    findall(Gun, best_close_quarters_weapon(Gun), BestCQ),
    write('  '), write(BestCQ), nl,

    write('Versatile weapons:'), nl,
    findall(Gun, versatile_weapons(Gun), Versatile),
    write('  '), write(Versatile), nl,

    write('Stealth build for M4A1:'), nl,
    stealth_build(m4a1, StealthBuild),
    write('  '), write(StealthBuild), nl,
    nl.

% Test attachment progression
test_attachment_progression :-
    write('Testing attachment progression:'), nl,
    write('Red dot sight unlock chain:'), nl,
    attachment_unlock_chain(red_dot_sight, AttChain),
    write('  '), write(AttChain), nl,

    write('Can red dot eventually unlock thermal scope?'), nl,
    (can_unlock_attachment_eventually(red_dot_sight, thermal_scope) ->
        write('  Yes') ; write('  No')), nl,
    nl.

% Test weapon statistics
test_weapon_statistics :-
    write('Testing weapon statistics:'), nl,
    write('Weapon counts by type:'), nl,
    count_weapons_by_type(assault_rifle, ARCount),
    count_weapons_by_type(pistol, PistolCount),
    count_weapons_by_type(shotgun, ShotgunCount),
    write('  Assault rifles: '), write(ARCount), nl,
    write('  Pistols: '), write(PistolCount), nl,
    write('  Shotguns: '), write(ShotgunCount), nl,

    write('Most common weapon type:'), nl,
    most_common_weapon_type(MostCommon),
    write('  '), write(MostCommon), nl,
    nl.

% Run all tests
run_all_tests :-
    write('=== Call of Duty Weapon Knowledgebase Tests ==='), nl, nl,
    test_gun_types,
    test_games,
    test_damage_levels,
    test_fire_rates,
    test_direct_unlock,
    test_recursive_unlock,
    test_unlock_chains,
    test_progression_depth,
    test_era_queries,
    test_utility_rules,
    test_longest_chains,
    test_new_weapon_types,
    test_weapon_classifications,
    test_attachment_system,
    test_game_mechanics,
    test_advanced_combinations,
    test_attachment_progression,
    test_weapon_statistics,
    write('=== Tests Complete ==='), nl.

% Example queries with expected outputs

% Query: Find all assault rifles
% Expected: List of all guns with type 'assault_rifle'
example_query_1 :-
    findall(Gun, gun_type(assault_rifle, Gun), AssaultRifles),
    write('All assault rifles: '), write(AssaultRifles), nl.

% Query: Find guns that can be unlocked from AK-47
% Expected: commando, galil (through recursive progression)
example_query_2 :-
    findall(Gun, can_unlock_eventually(ak47, Gun), UnlockableGuns),
    write('Guns unlockable from AK-47: '), write(UnlockableGuns), nl.

% Query: Find all high damage, fast firing guns
% Expected: Guns that are both high damage and fast firing
example_query_3 :-
    findall(Gun, (high_damage_guns(Gun), fast_firing_guns(Gun)), HighDamageFastGuns),
    write('High damage, fast firing guns: '), write(HighDamageFastGuns), nl.

% Query: Find progression depth of specific guns
% Expected: Numerical depth values
example_query_4 :-
    progression_depth(m4a1, Depth1),
    progression_depth(xm4, Depth2),
    write('M4A1 progression depth: '), write(Depth1), nl,
    write('XM4 progression depth: '), write(Depth2), nl.

% Interactive query examples for manual testing
% To run these manually in SWI-Prolog:
/*
?- gun_type(assault_rifle, Gun).
?- gun_in_game(modern_warfare, Gun).
?- can_unlock_eventually(m4a1, Gun).
?- unlock_chain(m1_garand, Chain).
?- progression_depth(xm4, Depth).
?- high_damage_guns(Gun).
?- modern_era_guns(Gun).
?- longest_chain_from(m1_garand, Chain).
*/
