% Call of Duty Weapon Knowledgebase
% Facts and rules for modeling weapons, their attributes, and progression

% Gun facts: gun(Name, Type, Ammo, DamageTier, FireRate, Game)
% DamageTier: low, medium, high, very_high
% FireRate: slow, medium, fast, very_fast

% =====================================
% CALL OF DUTY 1 (2003)
% =====================================
gun(m1_garand, rifle, '30-06', high, medium, call_of_duty_1).
gun(kar98k, rifle, '7.92mm', high, slow, call_of_duty_1).
gun(thompson, submachine_gun, '45_acp', medium, fast, call_of_duty_1).
gun(sten, submachine_gun, '9mm', medium, fast, call_of_duty_1).
gun(bar, light_machine_gun, '30-06', high, medium, call_of_duty_1).
gun(m1911, pistol, '45_acp', medium, medium, call_of_duty_1).
gun(trench_gun, shotgun, '12_gauge', very_high, slow, call_of_duty_1).

% =====================================
% CALL OF DUTY 2 (2005)
% =====================================
gun(mosin_nagant, rifle, '7.62x54r', high, slow, call_of_duty_2).
gun(ppsh_41, submachine_gun, '7.62x25', medium, very_fast, call_of_duty_2).
gun(mg42, machine_gun, '7.92mm', very_high, very_fast, call_of_duty_2).
gun(panzerfaust, rocket_launcher, rocket, very_high, slow, call_of_duty_2).
gun(luger, pistol, '9mm', medium, medium, call_of_duty_2).
gun(double_barrel, shotgun, '12_gauge', very_high, slow, call_of_duty_2).

% =====================================
% MODERN WARFARE (2007)
% =====================================
gun(m4a1, assault_rifle, '5.56mm', high, fast, modern_warfare).
gun(ak47, assault_rifle, '7.62mm', very_high, medium, modern_warfare).
gun(mp5, submachine_gun, '9mm', medium, fast, modern_warfare).
gun(m249_saw, light_machine_gun, '5.56mm', high, fast, modern_warfare).
gun(m40a3, sniper_rifle, '7.62mm', very_high, slow, modern_warfare).
gun(desert_eagle, pistol, '50_ae', very_high, slow, modern_warfare).
gun(w1200, shotgun, '12_gauge', very_high, slow, modern_warfare).

% =====================================
% MODERN WARFARE 2 (2009)
% =====================================
gun(acr, assault_rifle, '5.56mm', high, fast, modern_warfare_2).
gun(intervention, sniper_rifle, '408_cheytac', very_high, slow, modern_warfare_2).
gun(ump45, submachine_gun, '45_acp', medium, fast, modern_warfare_2).
gun(rpd, light_machine_gun, '7.62mm', very_high, medium, modern_warfare_2).
gun(usp_45, pistol, '45_acp', medium, medium, modern_warfare_2).
gun(aa12, shotgun, '12_gauge', high, fast, modern_warfare_2).

% =====================================
% BLACK OPS (2010)
% =====================================
gun(commando, assault_rifle, '5.56mm', high, fast, black_ops).
gun(ak74u, submachine_gun, '5.45mm', medium, fast, black_ops).
gun(galil, assault_rifle, '5.56mm', high, medium, black_ops).
gun(l96a1, sniper_rifle, '7.62mm', very_high, slow, black_ops).
gun(python, pistol, '357_magnum', high, slow, black_ops).
gun(spas12, shotgun, '12_gauge', very_high, slow, black_ops).

% =====================================
% MODERN WARFARE 3 (2011)
% =====================================
gun(mk14, assault_rifle, '7.62mm', very_high, slow, modern_warfare_3).
gun(mp7, submachine_gun, '4.6mm', medium, very_fast, modern_warfare_3).
gun(msr, sniper_rifle, '338_lapua', very_high, slow, modern_warfare_3).
gun(five_seven, pistol, '5.7mm', medium, fast, modern_warfare_3).
gun(model1887, shotgun, '12_gauge', very_high, slow, modern_warfare_3).

% =====================================
% BLACK OPS 2 (2012)
% =====================================
gun(an94, assault_rifle, '5.45mm', high, fast, black_ops_2).
gun(pdw57, submachine_gun, '5.7mm', medium, very_fast, black_ops_2).
gun(dsr50, sniper_rifle, '50_cal', very_high, slow, black_ops_2).
gun(b23r, pistol, '9mm', medium, fast, black_ops_2).
gun(r870, shotgun, '12_gauge', very_high, slow, black_ops_2).

% =====================================
% ADVANCED WARFARE (2014)
% =====================================
gun(bal27, assault_rifle, '5.56mm', high, fast, advanced_warfare).
gun(asm1, submachine_gun, '45_acp', medium, fast, advanced_warfare).
gun(mors, sniper_rifle, directed_energy, very_high, slow, advanced_warfare).
gun(pdw, pistol, directed_energy, medium, fast, advanced_warfare).
gun(bulldog, shotgun, '12_gauge', very_high, slow, advanced_warfare).

% =====================================
% BLACK OPS 3 (2015)
% =====================================
gun(icr1, assault_rifle, '5.56mm', medium, medium, black_ops_3).
gun(kuda, submachine_gun, '9mm', medium, fast, black_ops_3).
gun(locus, sniper_rifle, '7.62mm', very_high, slow, black_ops_3).
gun(mr6, pistol, '9mm', low, fast, black_ops_3).
gun(haymaker12, shotgun, '12_gauge', high, fast, black_ops_3).

% =====================================
% INFINITE WARFARE (2016)
% =====================================
gun(nv4, assault_rifle, '5.56mm', medium, medium, infinite_warfare).
gun(karma45, submachine_gun, '45_acp', medium, fast, infinite_warfare).
gun(kbs_longbow, sniper_rifle, '7.62mm', very_high, slow, infinite_warfare).
gun(erad, pistol, directed_energy, medium, fast, infinite_warfare).
gun(reaver, shotgun, '12_gauge', very_high, slow, infinite_warfare).

% =====================================
% WWII (2017)
% =====================================
gun(stg44, assault_rifle, '7.92mm', high, medium, wwii).
gun(grease_gun, submachine_gun, '45_acp', medium, medium, wwii).
gun(karabin, sniper_rifle, '7.62mm', very_high, slow, wwii).
gun(p08, pistol, '9mm', medium, medium, wwii).
gun(combat_shotgun, shotgun, '12_gauge', very_high, slow, wwii).

% =====================================
% BLACK OPS 4 (2018)
% =====================================
gun(icr7, assault_rifle, '5.56mm', medium, medium, black_ops_4).
gun(saug9mm, submachine_gun, '9mm', medium, very_fast, black_ops_4).
gun(paladin_hb50, sniper_rifle, '50_cal', very_high, slow, black_ops_4).
gun(strife, pistol, '9mm', medium, medium, black_ops_4).
gun(mog12, shotgun, '12_gauge', very_high, slow, black_ops_4).

% =====================================
% MODERN WARFARE (2019)
% =====================================
gun(m4a1_mw19, assault_rifle, '5.56mm', high, fast, modern_warfare_2019).
gun(mp5_mw19, submachine_gun, '9mm', medium, fast, modern_warfare_2019).
gun(ax50, sniper_rifle, '50_cal', very_high, slow, modern_warfare_2019).
gun(mk2_carbine, marksman_rifle, '45-70', high, slow, modern_warfare_2019).
gun(kar98k_mw, marksman_rifle, '7.92mm', very_high, slow, modern_warfare_2019).
gun(spr208, marksman_rifle, '300_norma', very_high, slow, modern_warfare_2019).
gun(fal, battle_rifle, '7.62mm', very_high, slow, modern_warfare_2019).
gun(oden, battle_rifle, '12.7mm', very_high, slow, modern_warfare_2019).
gun(fr556, battle_rifle, '5.56mm', high, medium, modern_warfare_2019).
gun(x16, pistol, '9mm', medium, fast, modern_warfare_2019).
gun(model680, shotgun, '12_gauge', very_high, slow, modern_warfare_2019).

% =====================================
% BLACK OPS COLD WAR (2020)
% =====================================
gun(ak74, assault_rifle, '5.45mm', high, medium, cold_war).
gun(mp5_cw, submachine_gun, '9mm', medium, fast, cold_war).
gun(lr_sniper, sniper_rifle, '7.62mm', very_high, slow, cold_war).
gun(dmr14, marksman_rifle, '7.62mm', high, medium, cold_war).
gun(type63, marksman_rifle, '7.62mm', high, medium, cold_war).
gun(m14, battle_rifle, '7.62mm', very_high, slow, cold_war).
gun(type63_br, battle_rifle, '7.62mm', high, medium, cold_war).
gun(diamatti, pistol, '9mm', medium, fast, cold_war).
gun(hauer77, shotgun, '12_gauge', very_high, slow, cold_war).

% =====================================
% VANGUARD (2021)
% =====================================
gun(stg44_vg, assault_rifle, '7.92mm', high, medium, vanguard).
gun(mp40_vg, submachine_gun, '9mm', medium, fast, vanguard).
gun(kar98k_vg, sniper_rifle, '7.92mm', very_high, slow, vanguard).
gun(g43, marksman_rifle, '7.92mm', high, medium, vanguard).
gun(svt40, marksman_rifle, '7.62x54r', high, medium, vanguard).
gun(automaton, battle_rifle, '6.5mm', high, fast, vanguard).
gun(bar_vg, battle_rifle, '30-06', very_high, medium, vanguard).
gun(machine_pistol, pistol, '9mm', medium, very_fast, vanguard).
gun(combat_shotgun_vg, shotgun, '12_gauge', very_high, slow, vanguard).

% =====================================
% MODERN WARFARE II (2022)
% =====================================
gun(m4_mw2, assault_rifle, '5.56mm', high, fast, modern_warfare_2_2022).
gun(lachmann_sub, submachine_gun, '9mm', medium, fast, modern_warfare_2_2022).
gun(la_b_330, sniper_rifle, '7.62mm', very_high, slow, modern_warfare_2_2022).
gun(sa_b50, marksman_rifle, '50_cal', very_high, slow, modern_warfare_2_2022).
gun(lockwood_mk2, marksman_rifle, '45-70', high, slow, modern_warfare_2_2022).
gun(ftac_recon, battle_rifle, '7.62mm', very_high, slow, modern_warfare_2_2022).
gun(lachmann762, battle_rifle, '7.62mm', very_high, slow, modern_warfare_2_2022).
gun(p890, pistol, '9mm', medium, medium, modern_warfare_2_2022).
gun(expedite12, shotgun, '12_gauge', very_high, slow, modern_warfare_2_2022).

% =====================================
% MODERN WARFARE III (2023)
% =====================================
gun(svoa_545, assault_rifle, '5.45mm', high, fast, modern_warfare_3_2023).
gun(wsm9, submachine_gun, '9mm', medium, very_fast, modern_warfare_3_2023).
gun(longbow_sniper, sniper_rifle, '7.62mm', very_high, slow, modern_warfare_3_2023).
gun(tempus_torrent, marksman_rifle, '7.62mm', high, medium, modern_warfare_3_2023).
gun(mcw_68, marksman_rifle, '7.62mm', high, medium, modern_warfare_3_2023).
gun(cronen_squall, battle_rifle, '6.5mm', high, fast, modern_warfare_3_2023).
gun(bas_b, battle_rifle, '338_lapua', very_high, slow, modern_warfare_3_2023).
gun(renetti, pistol, '9mm', medium, fast, modern_warfare_3_2023).
gun(lockwood300, shotgun, '12_gauge', very_high, slow, modern_warfare_3_2023).

% =====================================
% BLACK OPS 6 (2024)
% =====================================
gun(xm4, assault_rifle, '5.56mm', high, medium, black_ops_6).
gun(jackal_pdw, submachine_gun, '9mm', medium, fast, black_ops_6).
gun(lr_43_sniper, sniper_rifle, '7.62mm', very_high, slow, black_ops_6).
gun(ames85, marksman_rifle, '7.62mm', high, medium, black_ops_6).
gun(swat556, battle_rifle, '5.56mm', high, fast, black_ops_6).
gun(grekhova, pistol, '9mm', medium, medium, black_ops_6).
gun(marine_sp, shotgun, '12_gauge', very_high, slow, black_ops_6).

% Weapon unlock progression chains for recursive rules
unlock_progression(m1_garand, thompson).
unlock_progression(thompson, bar).
unlock_progression(kar98k, mosin_nagant).
unlock_progression(mosin_nagant, ppsh_41).
unlock_progression(ppsh_41, mg42).
unlock_progression(m4a1, acr).
unlock_progression(acr, mk14).
unlock_progression(ak47, commando).
unlock_progression(commando, galil).
unlock_progression(mp5, ump45).
unlock_progression(ump45, mp7).
unlock_progression(m40a3, intervention).
unlock_progression(intervention, msr).
unlock_progression(an94, bal27).
unlock_progression(bal27, icr1).
unlock_progression(icr1, nv4).
unlock_progression(nv4, stg44).
unlock_progression(stg44, icr7).
unlock_progression(icr7, m4a1_mw19).
unlock_progression(m4a1_mw19, ak74).
unlock_progression(ak74, stg44_vg).
unlock_progression(stg44_vg, m4_mw2).
unlock_progression(m4_mw2, svoa_545).
unlock_progression(svoa_545, xm4).

% Attachment unlock progression
unlock_progression(red_dot_sight, acog_scope).
unlock_progression(acog_scope, thermal_scope).
unlock_progression(foregrip, bipod).
unlock_progression(suppressor, compensator).

% ==============================
% WEAPON ATTACHMENTS SYSTEM
% ==============================

% Attachment facts: attachment(Name, Type, Effect, StatModifier, Game)
% Types: optic, barrel, stock, underbarrel, magazine, grip, muzzle, laser, perk
% Effects: damage_boost, accuracy_boost, range_boost, recoil_reduction, stealth, etc.
% StatModifier: numerical modifier (positive for boost, negative for penalty)

% Optic attachments
attachment(red_dot_sight, optic, target_acquisition, 15, all_games).
attachment(acog_scope, optic, long_range_precision, 25, all_games).
attachment(thermal_scope, optic, thermal_vision, 20, modern_games).
attachment(holographic_sight, optic, wide_view, 12, modern_games).
attachment(reflex_sight, optic, quick_ads, 18, all_games).
attachment(variable_zoom, optic, flexible_range, 30, advanced_games).
attachment(scout_scope, optic, medium_range, 22, classic_games).
attachment(iron_sights, optic, no_modification, 0, all_games).

% Barrel attachments
attachment(extended_barrel, barrel, range_boost, 25, all_games).
attachment(heavy_barrel, barrel, damage_boost, 20, all_games).
attachment(lightweight_barrel, barrel, mobility_boost, -10, modern_games).
attachment(suppressed_barrel, barrel, stealth, -5, modern_games).
attachment(compensated_barrel, barrel, recoil_reduction, 15, advanced_games).
attachment(fluted_barrel, barrel, velocity_boost, 18, modern_games).
attachment(reinforced_barrel, barrel, durability_boost, 10, all_games).

% Stock attachments
attachment(tactical_stock, stock, stability_boost, 15, all_games).
attachment(precision_stock, stock, accuracy_boost, 20, all_games).
attachment(lightweight_stock, stock, mobility_boost, 25, modern_games).
attachment(heavy_stock, stock, recoil_reduction, 18, all_games).
attachment(adjustable_stock, stock, versatility_boost, 12, advanced_games).
attachment(skeleton_stock, stock, ads_speed_boost, 22, modern_games).

% Underbarrel attachments
attachment(foregrip, underbarrel, recoil_reduction, 20, all_games).
attachment(bipod, underbarrel, stability_boost, 30, all_games).
attachment(grenade_launcher, underbarrel, explosive_capability, 50, classic_games).
attachment(laser_sight, underbarrel, hip_fire_accuracy, 25, modern_games).
attachment(angled_grip, underbarrel, ads_speed_boost, 15, modern_games).
attachment(vertical_grip, underbarrel, vertical_recoil_reduction, 18, all_games).

% Magazine attachments
attachment(extended_mag, magazine, ammo_capacity_boost, 50, all_games).
attachment(fast_mag, magazine, reload_speed_boost, 35, all_games).
attachment(dual_mag, magazine, reload_versatility, 25, modern_games).
attachment(drum_mag, magazine, high_capacity, 75, advanced_games).
attachment(speed_loader, magazine, reload_speed_boost, 40, classic_games).
attachment(jungle_style_mag, magazine, quick_reload, 30, all_games).

% Muzzle attachments
attachment(suppressor, muzzle, stealth, -10, all_games).
attachment(compensator, muzzle, recoil_reduction, 25, all_games).
attachment(muzzle_brake, muzzle, vertical_recoil_reduction, 20, modern_games).
attachment(flash_hider, muzzle, muzzle_flash_reduction, 15, all_games).
attachment(choke, muzzle, pellet_spread_reduction, 30, shotgun_specific).
attachment(monolithic_suppressor, muzzle, stealth_and_range, 20, modern_games).

% Laser attachments
attachment(tac_laser, laser, ads_speed_boost, 25, modern_games).
attachment(five_mw_laser, laser, hip_fire_accuracy, 30, modern_games).
attachment(one_mw_laser, laser, stealth_ads_boost, 15, modern_games).
attachment(visible_laser, laser, target_marking, 20, all_games).

% Perk attachments (weapon perks)
attachment(fmj, perk, penetration_boost, 40, all_games).
attachment(stopping_power, perk, damage_boost, 35, classic_games).
attachment(sleight_of_hand, perk, reload_speed_boost, 50, all_games).
attachment(steady_aim, perk, hip_fire_accuracy, 30, all_games).
attachment(deep_impact, perk, wall_penetration, 45, classic_games).
attachment(hardened, perk, bullet_penetration, 35, modern_games).

% Weapon-Attachment compatibility: weapon_attachment(WeaponName, AttachmentName, CompatibilityRating)
% CompatibilityRating: 1-5 (1=poor fit, 5=perfect fit)

% Assault Rifle attachments
weapon_attachment(m4a1, acog_scope, 5).
weapon_attachment(m4a1, foregrip, 4).
weapon_attachment(m4a1, suppressor, 5).
weapon_attachment(m4a1, extended_mag, 4).
weapon_attachment(m4a1, red_dot_sight, 5).
weapon_attachment(m4a1, tactical_stock, 4).

weapon_attachment(ak47, heavy_barrel, 5).
weapon_attachment(ak47, compensator, 5).
weapon_attachment(ak47, drum_mag, 4).
weapon_attachment(ak47, holographic_sight, 3).
weapon_attachment(ak47, heavy_stock, 5).

% Sniper Rifle attachments
weapon_attachment(intervention, variable_zoom, 5).
weapon_attachment(intervention, bipod, 5).
weapon_attachment(intervention, heavy_barrel, 4).
weapon_attachment(intervention, precision_stock, 5).

weapon_attachment(m40a3, scout_scope, 5).
weapon_attachment(m40a3, suppressor, 4).
weapon_attachment(m40a3, tactical_stock, 4).

% SMG attachments
weapon_attachment(mp5, red_dot_sight, 4).
weapon_attachment(mp5, suppressor, 5).
weapon_attachment(mp5, extended_mag, 5).
weapon_attachment(mp5, lightweight_stock, 4).
weapon_attachment(mp5, laser_sight, 5).

weapon_attachment(ump45, acog_scope, 3).
weapon_attachment(ump45, foregrip, 4).
weapon_attachment(ump45, fast_mag, 5).

% LMG attachments
weapon_attachment(m249_saw, bipod, 5).
weapon_attachment(m249_saw, heavy_barrel, 5).
weapon_attachment(m249_saw, extended_mag, 3).
weapon_attachment(m249_saw, holographic_sight, 4).

% Shotgun attachments
weapon_attachment(w1200, choke, 5).
weapon_attachment(w1200, extended_mag, 4).
weapon_attachment(w1200, laser_sight, 5).

% Pistol attachments
weapon_attachment(desert_eagle, red_dot_sight, 4).
weapon_attachment(desert_eagle, extended_mag, 3).
weapon_attachment(desert_eagle, laser_sight, 4).

% Game mechanics: game_mechanic(MechanicName, Type, Description, ApplicableGames)
% Types: progression, unlock, modification, gameplay, economy

% Progression mechanics
game_mechanic(weapon_leveling, progression, 'Weapons gain XP and unlock attachments', modern_games).
game_mechanic(prestige_system, progression, 'Reset level for prestige tokens', classic_games).
game_mechanic(weapon_mastery, progression, 'Complete challenges for camos', modern_games).
game_mechanic(battle_pass, progression, 'Seasonal progression system', recent_games).
game_mechanic(weapon_blueprints, progression, 'Pre-configured weapon variants', modern_games).
game_mechanic(camo_challenges, progression, 'Unlock cosmetic weapon skins', all_games).

% Unlock mechanics
game_mechanic(level_unlock, unlock, 'Weapons unlock by player level', all_games).
game_mechanic(challenge_unlock, unlock, 'Complete specific challenges', modern_games).
game_mechanic(currency_unlock, unlock, 'Purchase with in-game currency', advanced_games).
game_mechanic(attachment_progression, unlock, 'Use weapon to unlock attachments', modern_games).
game_mechanic(killstreak_unlock, unlock, 'Unlock through killstreaks', classic_games).

% Modification mechanics
game_mechanic(gunsmith, modification, 'Detailed weapon customization', modern_games).
game_mechanic(create_a_class, modification, 'Custom loadout creation', all_games).
game_mechanic(weapon_variants, modification, 'Statistical weapon variants', advanced_games).
game_mechanic(attachment_tuning, modification, 'Fine-tune attachment stats', recent_games).
game_mechanic(weapon_inspection, modification, 'View weapon in detail', modern_games).

% Gameplay mechanics
game_mechanic(weapon_swap, gameplay, 'Switch between primary/secondary', all_games).
game_mechanic(reload_cancel, gameplay, 'Cancel reload animation', modern_games).
game_mechanic(tactical_reload, gameplay, 'Retain ammo in chamber', modern_games).
game_mechanic(weapon_mounting, gameplay, 'Mount weapon on surfaces', recent_games).
game_mechanic(slide_cancel, gameplay, 'Cancel slide into other actions', recent_games).
game_mechanic(drop_shot, gameplay, 'Go prone while shooting', all_games).

% Economy mechanics
game_mechanic(weapon_trading, economy, 'Trade weapons between players', advanced_games).
game_mechanic(loot_boxes, economy, 'Random weapon/attachment rewards', controversial_games).
game_mechanic(direct_purchase, economy, 'Buy specific items', modern_games).
game_mechanic(salvage_system, economy, 'Break down items for currency', advanced_games).

% Rules for querying the knowledgebase

% Basic query rules
gun_type(Type, Gun) :-
    gun(Gun, Type, _, _, _, _).

gun_in_game(Game, Gun) :-
    gun(Gun, _, _, _, _, Game).

gun_damage(Damage, Gun) :-
    gun(Gun, _, _, Damage, _, _).

gun_fire_rate(FireRate, Gun) :-
    gun(Gun, _, _, _, FireRate, _).

gun_ammo_type(Ammo, Gun) :-
    gun(Gun, _, Ammo, _, _, _).

% Advanced query rules
high_damage_guns(Gun) :-
    gun(Gun, _, _, Damage, _, _),
    (Damage = high ; Damage = very_high).

fast_firing_guns(Gun) :-
    gun(Gun, _, _, _, FireRate, _),
    (FireRate = fast ; FireRate = very_fast).

modern_era_guns(Gun) :-
    gun(Gun, _, _, _, _, Game),
    member(Game, [modern_warfare, modern_warfare_2, modern_warfare_3,
                  modern_warfare_2019, modern_warfare_2_2022, modern_warfare_3_2023]).

classic_era_guns(Gun) :-
    gun(Gun, _, _, _, _, Game),
    member(Game, [call_of_duty_1, call_of_duty_2, wwii, vanguard]).

black_ops_series_guns(Gun) :-
    gun(Gun, _, _, _, _, Game),
    member(Game, [black_ops, black_ops_2, black_ops_3, black_ops_4, cold_war, black_ops_6]).

% Recursive rules for weapon progression

% Direct unlock relationship
can_unlock(BaseGun, NextGun) :-
    unlock_progression(BaseGun, NextGun).

% Recursive rule: can unlock through progression chain
can_unlock_eventually(BaseGun, TargetGun) :-
    unlock_progression(BaseGun, TargetGun).

can_unlock_eventually(BaseGun, TargetGun) :-
    unlock_progression(BaseGun, IntermediateGun),
    can_unlock_eventually(IntermediateGun, TargetGun).

% Find all guns in unlock chain starting from a base gun
unlock_chain(BaseGun, [BaseGun|Chain]) :-
    unlock_chain_helper(BaseGun, Chain).

unlock_chain_helper(Gun, []) :-
    \+ unlock_progression(Gun, _).

unlock_chain_helper(Gun, [NextGun|RestChain]) :-
    unlock_progression(Gun, NextGun),
    unlock_chain_helper(NextGun, RestChain).

% Count progression depth (recursive)
progression_depth(Gun, 0) :-
    \+ unlock_progression(_, Gun).

progression_depth(Gun, Depth) :-
    unlock_progression(PrevGun, Gun),
    progression_depth(PrevGun, PrevDepth),
    Depth is PrevDepth + 1.

% Find all guns that can eventually unlock a target gun
can_unlock_from(TargetGun, BaseGun) :-
    can_unlock_eventually(BaseGun, TargetGun).

% Utility rules
guns_by_era(Era, Guns) :-
    findall(Gun, (gun_in_game(Era, Gun)), Guns).

guns_by_type(Type, Guns) :-
    findall(Gun, gun_type(Type, Gun), Guns).

weapon_stats(Gun, Type, Ammo, Damage, FireRate, Game) :-
    gun(Gun, Type, Ammo, Damage, FireRate, Game).

% Advanced recursive rule: find longest unlock chain
longest_chain_from(Gun, Chain) :-
    findall(C, unlock_chain(Gun, C), Chains),
    max_length_list(Chains, Chain).

max_length_list([H], H).
max_length_list([H|T], Max) :-
    max_length_list(T, TMax),
    length(H, LenH),
    length(TMax, LenTMax),
    (LenH >= LenTMax -> Max = H ; Max = TMax).

% ==============================
% EXTENDED WEAPON TYPE RULES
% ==============================

% Query rules for new weapon types
pistol_guns(Gun) :-
    gun(Gun, pistol, _, _, _, _).

shotgun_guns(Gun) :-
    gun(Gun, shotgun, _, _, _, _).

marksman_rifle_guns(Gun) :-
    gun(Gun, marksman_rifle, _, _, _, _).

battle_rifle_guns(Gun) :-
    gun(Gun, battle_rifle, _, _, _, _).

% Secondary weapons (pistols and some SMGs)
secondary_weapon(Gun) :-
    gun(Gun, pistol, _, _, _, _).

secondary_weapon(Gun) :-
    gun(Gun, submachine_gun, _, _, _, Game),
    member(Game, [modern_warfare, modern_warfare_2, black_ops]).

% Primary weapons
primary_weapon(Gun) :-
    gun(Gun, Type, _, _, _, _),
    member(Type, [assault_rifle, sniper_rifle, light_machine_gun,
                  machine_gun, shotgun, marksman_rifle, battle_rifle]).

% Close quarters weapons
close_quarters_weapon(Gun) :-
    gun(Gun, Type, _, _, _, _),
    member(Type, [shotgun, submachine_gun, pistol]).

% Long range weapons
long_range_weapon(Gun) :-
    gun(Gun, Type, _, _, _, _),
    member(Type, [sniper_rifle, marksman_rifle, battle_rifle]).

% Automatic weapons
automatic_weapon(Gun) :-
    gun(Gun, Type, _, _, FireRate, _),
    member(Type, [assault_rifle, submachine_gun, light_machine_gun, machine_gun]),
    member(FireRate, [fast, very_fast]).

% ==============================
% ATTACHMENT SYSTEM RULES
% ==============================

% Basic attachment queries
attachment_type(Type, Attachment) :-
    attachment(Attachment, Type, _, _, _).

attachment_effect(Effect, Attachment) :-
    attachment(Attachment, _, Effect, _, _).

attachment_stat_modifier(Modifier, Attachment) :-
    attachment(Attachment, _, _, Modifier, _).

% Find attachments compatible with a weapon
compatible_attachments(Weapon, Attachment) :-
    weapon_attachment(Weapon, Attachment, _).

% Find highly compatible attachments (rating >= 4)
highly_compatible_attachments(Weapon, Attachment) :-
    weapon_attachment(Weapon, Attachment, Rating),
    Rating >= 4.

% Find the best attachment for a weapon (rating = 5)
best_attachments(Weapon, Attachment) :-
    weapon_attachment(Weapon, Attachment, 5).

% Optic attachments for a weapon
weapon_optics(Weapon, Optic) :-
    weapon_attachment(Weapon, Optic, _),
    attachment(Optic, optic, _, _, _).

% Performance enhancing attachments
damage_boosting_attachments(Attachment) :-
    attachment(Attachment, _, Effect, Modifier, _),
    (Effect = damage_boost ; Effect = penetration_boost),
    Modifier > 0.

accuracy_boosting_attachments(Attachment) :-
    attachment(Attachment, _, Effect, Modifier, _),
    (Effect = accuracy_boost ; Effect = recoil_reduction),
    Modifier > 0.

stealth_attachments(Attachment) :-
    attachment(Attachment, _, stealth, _, _).

% ==============================
% GAME MECHANICS RULES
% ==============================

% Game mechanic queries
mechanic_type(Type, Mechanic) :-
    game_mechanic(Mechanic, Type, _, _).

progression_mechanics(Mechanic) :-
    game_mechanic(Mechanic, progression, _, _).

unlock_mechanics(Mechanic) :-
    game_mechanic(Mechanic, unlock, _, _).

modification_mechanics(Mechanic) :-
    game_mechanic(Mechanic, modification, _, _).

gameplay_mechanics(Mechanic) :-
    game_mechanic(Mechanic, gameplay, _, _).

economy_mechanics(Mechanic) :-
    game_mechanic(Mechanic, economy, _, _).

% Modern game mechanics
modern_mechanics(Mechanic) :-
    game_mechanic(Mechanic, _, _, Games),
    member(Games, [modern_games, advanced_games, recent_games]).

% Classic game mechanics
classic_mechanics(Mechanic) :-
    game_mechanic(Mechanic, _, _, classic_games).

% ==============================
% ADVANCED COMBINATION RULES
% ==============================

% Weapons with optimal loadouts (have 3+ compatible attachments)
well_supported_weapons(Weapon) :-
    gun(Weapon, _, _, _, _, _),
    findall(A, compatible_attachments(Weapon, A), Attachments),
    length(Attachments, Count),
    Count >= 3.

% Weapons optimized for specific roles
sniper_setup(Weapon, Optic, Barrel, Stock) :-
    gun(Weapon, sniper_rifle, _, _, _, _),
    weapon_attachment(Weapon, Optic, _),
    attachment(Optic, optic, Effect, _, _),
    member(Effect, [long_range_precision, variable_zoom]),
    weapon_attachment(Weapon, Barrel, _),
    attachment(Barrel, barrel, range_boost, _, _),
    weapon_attachment(Weapon, Stock, _),
    attachment(Stock, stock, stability_boost, _, _).

assault_setup(Weapon, Optic, Grip, Muzzle) :-
    gun(Weapon, assault_rifle, _, _, _, _),
    weapon_attachment(Weapon, Optic, _),
    attachment(Optic, optic, target_acquisition, _, _),
    weapon_attachment(Weapon, Grip, _),
    attachment(Grip, underbarrel, recoil_reduction, _, _),
    weapon_attachment(Weapon, Muzzle, _),
    attachment(Muzzle, muzzle, _, _, _).

stealth_setup(Weapon, Suppressor, Optic) :-
    gun(Weapon, _, _, _, _, _),
    weapon_attachment(Weapon, Suppressor, _),
    attachment(Suppressor, muzzle, stealth, _, _),
    weapon_attachment(Weapon, Optic, _),
    attachment(Optic, optic, _, _, _).

% ==============================
% RECURSIVE ATTACHMENT PROGRESSION
% ==============================

% Recursive attachment unlock chains
attachment_unlock_chain(BaseAttachment, [BaseAttachment|Chain]) :-
    attachment_unlock_helper(BaseAttachment, Chain).

attachment_unlock_helper(Attachment, []) :-
    \+ unlock_progression(Attachment, _).

attachment_unlock_helper(Attachment, [NextAttachment|RestChain]) :-
    unlock_progression(Attachment, NextAttachment),
    attachment_unlock_helper(NextAttachment, RestChain).

% Can eventually unlock attachment through progression
can_unlock_attachment_eventually(BaseAttachment, TargetAttachment) :-
    unlock_progression(BaseAttachment, TargetAttachment).

can_unlock_attachment_eventually(BaseAttachment, TargetAttachment) :-
    unlock_progression(BaseAttachment, IntermediateAttachment),
    can_unlock_attachment_eventually(IntermediateAttachment, TargetAttachment).

% ==============================
% WEAPON STATISTICS AND ANALYSIS
% ==============================

% Count weapons by type
count_weapons_by_type(Type, Count) :-
    findall(Gun, gun_type(Type, Gun), Guns),
    length(Guns, Count).

% Find most common weapon type
most_common_weapon_type(Type) :-
    findall(T-C, (gun_type(T, _), count_weapons_by_type(T, C)), TypeCounts),
    max_member(_-MaxCount, TypeCounts),
    member(Type-MaxCount, TypeCounts).

% Weapons with unique characteristics
unique_ammo_weapons(Gun, Ammo) :-
    gun(Gun, _, Ammo, _, _, _),
    \+ (gun(OtherGun, _, Ammo, _, _, _), Gun \= OtherGun).

% Era progression analysis
era_weapon_count(Era, Count) :-
    findall(Gun, (
        member(Era, [classic, modern, advanced]),
        (Era = classic -> classic_era_guns(Gun);
         Era = modern -> modern_era_guns(Gun);
         Era = advanced -> advanced_era_guns(Gun))
    ), Guns),
    length(Guns, Count).

advanced_era_guns(Gun) :-
    gun(Gun, _, _, _, _, Game),
    member(Game, [advanced_warfare, infinite_warfare, black_ops_3]).

% ==============================
% COMPLEX QUERY COMBINATIONS
% ==============================

% Best weapons for specific scenarios
best_close_quarters_weapon(Gun) :-
    close_quarters_weapon(Gun),
    high_damage_guns(Gun),
    fast_firing_guns(Gun).

best_long_range_weapon(Gun) :-
    long_range_weapon(Gun),
    high_damage_guns(Gun),
    gun(Gun, _, _, _, _, _).

versatile_weapons(Gun) :-
    gun(Gun, assault_rifle, _, _, _, _),
    high_damage_guns(Gun),
    well_supported_weapons(Gun).

% Weapon recommendations by game
recommended_weapons_for_game(Game, Weapons) :-
    findall(Gun, (
        gun(Gun, _, _, _, _, Game),
        high_damage_guns(Gun),
        well_supported_weapons(Gun)
    ), Weapons).

% ==============================
% ATTACHMENT OPTIMIZATION RULES
% ==============================

% Find optimal attachment combinations
optimal_damage_build(Weapon, Attachments) :-
    gun(Weapon, _, _, _, _, _),
    findall(A, (
        weapon_attachment(Weapon, A, Rating),
        damage_boosting_attachments(A),
        Rating >= 4
    ), Attachments).

optimal_accuracy_build(Weapon, Attachments) :-
    gun(Weapon, _, _, _, _, _),
    findall(A, (
        weapon_attachment(Weapon, A, Rating),
        accuracy_boosting_attachments(A),
        Rating >= 4
    ), Attachments).

stealth_build(Weapon, Attachments) :-
    gun(Weapon, _, _, _, _, _),
    findall(A, (
        weapon_attachment(Weapon, A, _),
        stealth_attachments(A)
    ), Attachments).
