% Quick validation script for the knowledgebase
:- consult('src/weapons.pl').

% Test basic facts
test_basic_facts :-
    gun(m4a1, assault_rifle, '5.56mm', high, fast, modern_warfare),
    write('✓ Basic gun facts working'), nl.

% Test basic rules
test_basic_rules :-
    gun_type(assault_rifle, m4a1),
    gun_in_game(modern_warfare, m4a1),
    write('✓ Basic query rules working'), nl.

% Test recursive rules
test_recursive_rules :-
    can_unlock_eventually(m4a1, acr),
    write('✓ Recursive unlock progression working'), nl.

% Test progression depth
test_progression_depth :-
    progression_depth(m4a1, 0),
    write('✓ Progression depth calculation working'), nl.

% Test unlock chains
test_unlock_chains :-
    unlock_chain(m4a1, [m4a1, acr, mk14]),
    write('✓ Unlock chain generation working'), nl.

% Run all validation tests
validate_knowledgebase :-
    write('=== Validating Call of Duty Weapon Knowledgebase ==='), nl,
    test_basic_facts,
    test_basic_rules,
    test_recursive_rules,
    test_progression_depth,
    test_unlock_chains,
    write('=== All validations passed! ==='), nl.

% Auto-run validation and halt
:- initialization(validate_knowledgebase), halt.
