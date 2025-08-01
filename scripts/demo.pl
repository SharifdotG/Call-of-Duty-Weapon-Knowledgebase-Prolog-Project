% Demonstration script for Call of Duty Weapon Knowledgebase
% This script demonstrates key features and recursive functionality

:- consult('src/weapons.pl').

% Demonstrate basic queries
demo_basic_queries :-
    write('=== BASIC QUERIES DEMONSTRATION ==='), nl, nl,

    write('1. Finding all assault rifles:'), nl,
    findall(Gun, gun_type(assault_rifle, Gun), AssaultRifles),
    length(AssaultRifles, ARCount),
    write('   Found '), write(ARCount), write(' assault rifles: '),
    write(AssaultRifles), nl, nl,

    write('2. Finding guns in Modern Warfare (2007):'), nl,
    findall(Gun, gun_in_game(modern_warfare, Gun), MWGuns),
    write('   Guns: '), write(MWGuns), nl, nl,

    write('3. Finding high damage guns:'), nl,
    findall(Gun, high_damage_guns(Gun), HighDamageGuns),
    length(HighDamageGuns, HDCount),
    write('   Found '), write(HDCount), write(' high damage guns'), nl, nl.

% Demonstrate recursive queries
demo_recursive_queries :-
    write('=== RECURSIVE QUERIES DEMONSTRATION ==='), nl, nl,

    write('1. Direct unlock progression from M4A1:'), nl,
    (can_unlock(m4a1, NextGun) ->
        write('   M4A1 can directly unlock: '), write(NextGun), nl
    ;   write('   M4A1 cannot directly unlock any gun'), nl
    ), nl,

    write('2. Complete unlock chain from M4A1:'), nl,
    unlock_chain(m4a1, Chain),
    write('   Chain: '), write(Chain), nl, nl,

    write('3. Eventual unlock possibilities from M4A1:'), nl,
    findall(Gun, can_unlock_eventually(m4a1, Gun), EventualUnlocks),
    write('   Can eventually unlock: '), write(EventualUnlocks), nl, nl,

    write('4. Progression depths of final guns in chains:'), nl,
    progression_depth(mk14, MK14Depth),
    progression_depth(xm4, XM4Depth),
    write('   MK14 depth: '), write(MK14Depth), nl,
    write('   XM4 depth: '), write(XM4Depth), nl, nl.

% Demonstrate classification queries
demo_classification_queries :-
    write('=== CLASSIFICATION QUERIES DEMONSTRATION ==='), nl, nl,

    write('1. Modern era guns (sample):'), nl,
    findall(Gun, modern_era_guns(Gun), ModernGuns),
    length(ModernGuns, ModernCount),
    write('   Found '), write(ModernCount), write(' modern era guns'), nl,

    write('2. Black Ops series guns (sample):'), nl,
    findall(Gun, black_ops_series_guns(Gun), BOGuns),
    length(BOGuns, BOCount),
    write('   Found '), write(BOCount), write(' Black Ops series guns'), nl,

    write('3. Fast firing, high damage guns:'), nl,
    findall(Gun, (fast_firing_guns(Gun), high_damage_guns(Gun)), FastHighDamage),
    write('   Fast & high damage: '), write(FastHighDamage), nl, nl.

% Demonstrate advanced recursive features
demo_advanced_features :-
    write('=== ADVANCED RECURSIVE FEATURES ==='), nl, nl,

    write('1. Longest unlock chain from M1 Garand:'), nl,
    longest_chain_from(m1_garand, LongestChain),
    write('   Chain: '), write(LongestChain), nl,
    length(LongestChain, ChainLength),
    write('   Length: '), write(ChainLength), write(' weapons'), nl, nl,

    write('2. Finding all base guns (depth 0):'), nl,
    findall(Gun, progression_depth(Gun, 0), BaseGuns),
    write('   Base guns: '), write(BaseGuns), nl, nl,

    write('3. Guns that can unlock XM4:'), nl,
    findall(Gun, can_unlock_eventually(Gun, xm4), CanUnlockXM4),
    write('   Can unlock XM4: '), write(CanUnlockXM4), nl, nl.

% Demonstrate new weapon types
demo_new_weapon_types :-
    write('=== NEW WEAPON TYPES DEMONSTRATION ==='), nl, nl,

    write('1. Extended weapon arsenal:'), nl,
    findall(Gun, pistol_guns(Gun), Pistols),
    length(Pistols, PistolCount),
    write('   Pistols: '), write(PistolCount), write(' weapons'), nl,

    findall(Gun, shotgun_guns(Gun), Shotguns),
    length(Shotguns, ShotgunCount),
    write('   Shotguns: '), write(ShotgunCount), write(' weapons'), nl,

    findall(Gun, marksman_rifle_guns(Gun), MarksmanRifles),
    length(MarksmanRifles, MRCount),
    write('   Marksman rifles: '), write(MRCount), write(' weapons'), nl,

    findall(Gun, battle_rifle_guns(Gun), BattleRifles),
    length(BattleRifles, BRCount),
    write('   Battle rifles: '), write(BRCount), write(' weapons'), nl, nl,

    write('2. Weapon classifications:'), nl,
    findall(Gun, primary_weapon(Gun), PrimaryWeapons),
    length(PrimaryWeapons, PrimaryCount),
    write('   Primary weapons: '), write(PrimaryCount), nl,

    findall(Gun, secondary_weapon(Gun), SecondaryWeapons),
    length(SecondaryWeapons, SecondaryCount),
    write('   Secondary weapons: '), write(SecondaryCount), nl,

    write('3. Best close quarters weapons:'), nl,
    findall(Gun, best_close_quarters_weapon(Gun), BestCQ),
    write('   '), write(BestCQ), nl, nl.

% Demonstrate attachment system
demo_attachment_system :-
    write('=== ATTACHMENT SYSTEM DEMONSTRATION ==='), nl, nl,

    write('1. Attachment categories:'), nl,
    findall(Att, attachment_type(optic, Att), Optics),
    length(Optics, OpticCount),
    write('   Optic attachments: '), write(OpticCount), nl,

    findall(Att, attachment_type(barrel, Att), Barrels),
    length(Barrels, BarrelCount),
    write('   Barrel attachments: '), write(BarrelCount), nl,

    findall(Att, attachment_type(muzzle, Att), Muzzles),
    length(Muzzles, MuzzleCount),
    write('   Muzzle attachments: '), write(MuzzleCount), nl, nl,

    write('2. M4A1 attachment compatibility:'), nl,
    findall(Att, compatible_attachments(m4a1, Att), M4Attachments),
    write('   Compatible: '), write(M4Attachments), nl,

    findall(Att, best_attachments(m4a1, Att), M4BestAtts),
    write('   Best rated: '), write(M4BestAtts), nl, nl,

    write('3. Specialized attachment builds:'), nl,
    stealth_build(m4a1, StealthBuild),
    write('   M4A1 stealth build: '), write(StealthBuild), nl,

    optimal_damage_build(ak47, DamageBuild),
    write('   AK-47 damage build: '), write(DamageBuild), nl, nl.

% Demonstrate game mechanics
demo_game_mechanics :-
    write('=== GAME MECHANICS DEMONSTRATION ==='), nl, nl,

    write('1. Progression systems:'), nl,
    findall(Mech, progression_mechanics(Mech), ProgMechanics),
    length(ProgMechanics, ProgCount),
    write('   Progression mechanics: '), write(ProgCount), nl,
    write('   Examples: '), write(ProgMechanics), nl, nl,

    write('2. Modern vs Classic mechanics:'), nl,
    findall(Mech, modern_mechanics(Mech), ModernMechs),
    length(ModernMechs, ModernCount),
    write('   Modern mechanics: '), write(ModernCount), nl,

    findall(Mech, classic_mechanics(Mech), ClassicMechs),
    length(ClassicMechs, ClassicCount),
    write('   Classic mechanics: '), write(ClassicCount), nl, nl,

    write('3. Unlock systems:'), nl,
    findall(Mech, unlock_mechanics(Mech), UnlockMechs),
    write('   Unlock mechanics: '), write(UnlockMechs), nl, nl.

% Demonstrate advanced weapon analysis
demo_advanced_analysis :-
    write('=== ADVANCED WEAPON ANALYSIS ==='), nl, nl,

    write('1. Well-supported weapons (3+ attachments):'), nl,
    findall(Gun, well_supported_weapons(Gun), WellSupported),
    length(WellSupported, WSCount),
    write('   Count: '), write(WSCount), write(' weapons'), nl,

    write('2. Versatile assault rifles:'), nl,
    findall(Gun, versatile_weapons(Gun), Versatile),
    write('   Versatile ARs: '), write(Versatile), nl, nl,

    write('3. Weapon statistics:'), nl,
    most_common_weapon_type(MostCommon),
    write('   Most common type: '), write(MostCommon), nl,

    count_weapons_by_type(assault_rifle, ARCount),
    count_weapons_by_type(pistol, PistolCount),
    write('   Assault rifles: '), write(ARCount), nl,
    write('   Pistols: '), write(PistolCount), nl, nl,

    write('4. Attachment progression chains:'), nl,
    attachment_unlock_chain(red_dot_sight, AttChain),
    write('   Red dot sight chain: '), write(AttChain), nl, nl.% Main demonstration
run_demonstration :-
    write('Call of Duty Weapon Knowledgebase - Extended Feature Demonstration'), nl,
    write('================================================================'), nl, nl,
    demo_basic_queries,
    demo_recursive_queries,
    demo_classification_queries,
    demo_advanced_features,
    demo_new_weapon_types,
    demo_attachment_system,
    demo_game_mechanics,
    demo_advanced_analysis,
    write('=== DEMONSTRATION COMPLETE ==='), nl,
    write('The extended knowledgebase successfully demonstrates:'), nl,
    write('- Basic fact querying (61+ weapons)'), nl,
    write('- Recursive progression rules'), nl,
    write('- Complex classification rules'), nl,
    write('- Advanced recursive features'), nl,
    write('- Extended weapon types (pistols, shotguns, marksman rifles, etc.)'), nl,
    write('- Comprehensive attachment system (50+ attachments)'), nl,
    write('- Game mechanics modeling (20+ mechanics)'), nl,
    write('- Advanced weapon analysis and optimization'), nl,
    write('- Attachment progression and compatibility'), nl, nl.

% Auto-run demonstration
:- initialization(run_demonstration).
