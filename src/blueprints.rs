use rand::seq::SliceRandom;

use crate::*;

#[derive(Resource)]
pub struct Blueprints {
    pub units: Vec<Unit>,
}

impl Blueprints {
    pub fn construct() -> Self {
        // archetypes
        // - weak units
        // - just attack
        // - buffing
        // - healing
        // - self damage
        // - energy gain infinite -> careful, should be hard to assemble
        // - converters life -> attack, energy -> life, ...
        // - area of effect
        // - being targeted, responses
        // - drain the team
        // - counterattack
        // - lifelink
        // - high cost high damage

        // shorthands (kinda macros)
        let life = || UnitValues::Life;
        let attack = || UnitValues::Attack;
        let energy = || UnitValues::Energy;

        let imm = |amt| CombatNumberSource::Immediate(amt);
        let src_unit = |v| CombatNumberSource::Unit(v);
        let src_unit_neg = |v| CombatNumberSource::UnitNegated(v);

        let num = |s, v| CombatNumber {
            source: s,
            values: v,
        };

        let fx = |target, gains| CombatEffect { target, gains };

        // effects shorthands
        let damage_mostlife_withattack = CombatEffect {
            target: CombatTarget::EnemyWithMost(UnitValues::Life),
            gains: vec![num(src_unit_neg(attack()), life())],
        };
        let damage_mostenergy_withattack = CombatEffect {
            target: CombatTarget::EnemyWithMost(UnitValues::Energy),
            gains: vec![num(src_unit_neg(attack()), life())],
        };
        let gain_self = |buffs: Vec<CombatNumber>| CombatEffect {
            target: CombatTarget::This,
            gains: buffs,
        };
        let gain_enemy_life = |amt| CombatEffect {
            target: CombatTarget::AllEnemy,
            gains: vec![CombatNumber {
                source: CombatNumberSource::Immediate(amt),
                values: UnitValues::Life,
            }],
        };
        let gain_ally_life = |amt| CombatEffect {
            target: CombatTarget::AllAlly,
            gains: vec![CombatNumber {
                source: CombatNumberSource::Immediate(amt),
                values: UnitValues::Life,
            }],
        };
        let gain_ally_energy = |amt| CombatEffect {
            target: CombatTarget::AllAlly,
            gains: vec![CombatNumber {
                source: CombatNumberSource::Immediate(amt),
                values: UnitValues::Energy,
            }],
        };
        let gain_abilitysource_attack = |amt| CombatEffect {
            target: CombatTarget::AbilityTarget,
            gains: vec![num(imm(amt), attack())],
        };
        let gain_abilitysource_attack = |amt| CombatEffect {
            target: CombatTarget::AbilityTarget,
            gains: vec![num(imm(amt), attack())],
        };
        let gain_ally_attack = |amt| CombatEffect {
            target: CombatTarget::AllAlly,
            gains: vec![CombatNumber {
                source: CombatNumberSource::Immediate(amt),
                values: UnitValues::Attack,
            }],
        };
        let damage_mostlife_imm = |amt| CombatEffect {
            target: CombatTarget::EnemyWithMost(UnitValues::Life),
            gains: vec![CombatNumber {
                source: CombatNumberSource::Immediate(amt),
                values: UnitValues::Life,
            }],
        };
        let drain_mostattack_imm = |amt| CombatEffect {
            target: CombatTarget::EnemyWithMost(UnitValues::Attack),
            gains: vec![CombatNumber {
                source: CombatNumberSource::Immediate(amt),
                values: UnitValues::Energy,
            }],
        };

        // trigger shorthands
        let on_other_loss = |v| CombatTrigger {
            target: CombatTarget::AllOther,
            watch: CombatTriggerWatch::ValueDecrease(v),
        };
        let on_otherally_lifeloss = CombatTrigger {
            target: CombatTarget::AllOtherAlly,
            watch: CombatTriggerWatch::ValueDecrease(UnitValues::Life),
        };
        let on_self_lifeloss = CombatTrigger {
            target: CombatTarget::This,
            watch: CombatTriggerWatch::ValueDecrease(UnitValues::Life),
        };
        let on_self_attackloss = CombatTrigger {
            target: CombatTarget::This,
            watch: CombatTriggerWatch::ValueDecrease(UnitValues::Attack),
        };

        // crazy units
        let units = vec![
            Unit {
                name: format!("Lucilla"),
                sprite_index: 2,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(24),
                attack: UnitValue::full(5),
                energy: UnitValue::full(7),
                owner: Owner::Player,
                abilities: vec![CombatAbility {
                    effects: vec![damage_mostlife_withattack.clone()],
                    costs: vec![num(imm(5), energy())],
                    trigger: None,
                }],
            },
            Unit {
                name: format!("Ramona"),
                sprite_index: 6,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(18),
                attack: UnitValue::full(1),
                energy: UnitValue::full(10),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![gain_ally_life(1)],
                        costs: vec![num(imm(10), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![damage_mostlife_withattack.clone()],
                        costs: vec![num(imm(5), energy())],
                        trigger: None,
                    },
                ],
            },
            Unit {
                name: format!("Sparks"),
                sprite_index: 0,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(15),
                attack: UnitValue::full(1),
                energy: UnitValue::full(16),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![damage_mostlife_imm(-2)],
                    costs: vec![num(imm(5), energy())],
                    trigger: None,
                }],
            },
            Unit {
                name: format!("Thaunos"),
                sprite_index: 11,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(30),
                attack: UnitValue::full(2),
                energy: UnitValue::full(6),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![gain_self(vec![num(imm(2), attack())])],
                        costs: vec![num(imm(3), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![damage_mostlife_withattack.clone()],
                        costs: vec![num(imm(3), energy())],
                        trigger: None,
                    },
                ],
            },
            Unit {
                name: format!("Piston"),
                sprite_index: 11,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(20),
                attack: UnitValue::full(2),
                energy: UnitValue::full(20),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![gain_ally_energy(1)],
                        costs: vec![num(imm(16), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![drain_mostattack_imm(-2)],
                        costs: vec![num(imm(3), energy())],
                        trigger: None,
                    },
                ],
            },
            Unit {
                name: format!("Dorm"),
                sprite_index: 7,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(20),
                attack: UnitValue::full(2),
                energy: UnitValue::full(20),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![gain_abilitysource_attack(2)],
                    costs: vec![num(imm(5), energy())],
                    trigger: Some(on_otherally_lifeloss.clone()),
                }],
            },
            Unit {
                name: format!("Candle"),
                sprite_index: 12,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(7),
                attack: UnitValue::full(1),
                energy: UnitValue::full(12),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![gain_self(vec![num(imm(-3), life())]), gain_enemy_life(-1)],
                        costs: vec![num(imm(6), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![
                            fx(CombatTarget::AllOtherAlly, vec![num(imm(2), energy())]),
                            fx(
                                CombatTarget::EnemyWithMost(energy()),
                                vec![num(imm(-4), energy())],
                            ),
                        ],
                        costs: vec![num(imm(6), energy())],
                        trigger: Some(on_self_lifeloss.clone()),
                    },
                ],
            },
            Unit {
                name: format!("Carmine"),
                sprite_index: 8,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(12),
                attack: UnitValue::full(1),
                energy: UnitValue::full(24),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![gain_ally_attack(1)],
                        costs: vec![num(imm(10), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![gain_self(vec![num(imm(-10), life())]), gain_ally_attack(4)],
                        costs: vec![num(imm(10), energy())],
                        trigger: Some(on_self_attackloss.clone()),
                    },
                ],
            },
            Unit {
                name: format!("Walter"),
                sprite_index: 1,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(24),
                attack: UnitValue::full(5),
                energy: UnitValue::full(3),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![damage_mostenergy_withattack.clone()],
                    costs: vec![num(imm(5), energy())],
                    trigger: None,
                }],
            },
            Unit {
                name: format!("Gray Eminence"),
                sprite_index: 10,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(3),
                attack: UnitValue::full(1),
                energy: UnitValue::full(10),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AbilityTarget, vec![num(imm(-1), life())])],
                        costs: vec![num(imm(1), energy())],
                        trigger: Some(on_other_loss(attack())),
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(attack()),
                            vec![num(imm(-1), attack())],
                        )],
                        costs: vec![num(imm(5), energy())],
                        trigger: None,
                    },
                ],
            },
            Unit {
                name: format!("Gorb"),
                sprite_index: 4,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(17),
                attack: UnitValue::full(3),
                energy: UnitValue::full(8),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AllAlly, vec![num(imm(-1), life())])],
                        costs: vec![num(imm(5), energy())],
                        trigger: Some(on_other_loss(attack())),
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(attack()),
                            vec![num(imm(-2), attack()), num(imm(-1), life())],
                        )],
                        costs: vec![num(imm(1), energy())],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllOtherAlly,
                            watch: CombatTriggerWatch::ValueDecrease(life()),
                        }),
                    },
                ],
            },
        ];

        let units = vec![
            // basic attack
            Unit {
                name: format!("Brandon"),
                sprite_index: 1,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(12),
                attack: UnitValue::full(5),
                energy: UnitValue::full(10),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![fx(
                        CombatTarget::EnemyWithMost(life()),
                        vec![num(src_unit_neg(attack()), life())],
                    )],
                    costs: vec![num(imm(10), energy())],
                    trigger: None,
                }],
            },
            // basic attack to damage, buffs itself
            Unit {
                name: format!("Spark"),
                sprite_index: 0,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(10),
                attack: UnitValue::full(4),
                energy: UnitValue::full(4),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::This, vec![num(imm(1), attack())])],
                        costs: vec![num(imm(1), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(life()),
                            vec![num(imm(-1), life())],
                        )],
                        costs: vec![num(imm(1), attack())],
                        trigger: None,
                    },
                ],
            },
            // heal 1 aoe
            Unit {
                name: format!("Elena"),
                sprite_index: 6,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(12),
                attack: UnitValue::full(0),
                energy: UnitValue::full(4),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![fx(CombatTarget::AllAlly, vec![num(imm(1), life())])],
                    costs: vec![num(imm(2), energy())],
                    trigger: None,
                }],
            },
            // enemy lifeloss is attack, good damager
            Unit {
                name: format!("Jade"),
                sprite_index: 2,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(8),
                attack: UnitValue::full(0),
                energy: UnitValue::full(2),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(life()),
                            vec![num(imm(-2), life())],
                        )],
                        costs: vec![num(imm(1), attack()), num(imm(1), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![fx(CombatTarget::This, vec![num(imm(1), attack())])],
                        costs: vec![],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllEnemy,
                            watch: CombatTriggerWatch::ValueDecrease(life()),
                        }),
                    },
                ],
            },
            // ally lifeloss is 1 atk to them, bad atk to damage
            Unit {
                name: format!("Kramer"),
                sprite_index: 3,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(18),
                attack: UnitValue::full(4),
                energy: UnitValue::full(0),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AbilityTarget, vec![num(imm(1), attack())])],
                        costs: vec![],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllAlly,
                            watch: CombatTriggerWatch::ValueDecrease(life()),
                        }),
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(energy()),
                            vec![num(imm(-1), life())],
                        )],
                        costs: vec![num(imm(2), attack())],
                        trigger: None,
                    },
                ],
            },
            // ally lifegain is 1 atk to them, bad healer
            Unit {
                name: format!("Poppy"),
                sprite_index: 5,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(12),
                attack: UnitValue::full(4),
                energy: UnitValue::full(10),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AbilityTarget, vec![num(imm(1), attack())])],
                        costs: vec![num(imm(3), energy())],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllOtherAlly,
                            watch: CombatTriggerWatch::ValueIncrease(life()),
                        }),
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::AllyWithLeast(life()),
                            vec![num(imm(3), life())],
                        )],
                        costs: vec![num(imm(2), energy())],
                        trigger: None,
                    },
                ],
            },
            // ally gainattack is scaling damage
            Unit {
                name: format!("Roth"),
                sprite_index: 7,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(8),
                attack: UnitValue::full(2),
                energy: UnitValue::full(8),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![fx(
                        CombatTarget::EnemyWithMost(life()),
                        vec![num(src_unit_neg(attack()), life())],
                    )],
                    costs: vec![num(imm(4), energy())],
                    trigger: Some(CombatTrigger {
                        target: CombatTarget::AllAlly,
                        watch: CombatTriggerWatch::ValueIncrease(attack()),
                    }),
                }],
            },
            // enraged, when hit buffs
            Unit {
                name: format!("Scarlet"),
                sprite_index: 8,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(24),
                attack: UnitValue::full(1),
                energy: UnitValue::full(8),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![
                            fx(
                                CombatTarget::AllyWithMost(life()),
                                vec![num(imm(1), attack())],
                            ),
                            fx(CombatTarget::This, vec![num(imm(4), attack())]),
                        ],
                        costs: vec![num(imm(1), energy())],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::This,
                            watch: CombatTriggerWatch::ValueDecrease(life()),
                        }),
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(attack()),
                            vec![num(imm(-1), life())],
                        )],
                        costs: vec![num(imm(2), attack())],
                        trigger: None,
                    },
                ],
            },
            // bloodletting for attack
            Unit {
                name: format!("Preston"),
                sprite_index: 14,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(10),
                attack: UnitValue::full(0),
                energy: UnitValue::full(10),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AllOther, vec![num(imm(1), attack())])],
                        costs: vec![num(imm(2), life()), num(imm(4), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(life()),
                            vec![num(imm(-2), life())],
                        )],
                        costs: vec![num(imm(3), attack())],
                        trigger: None,
                    },
                ],
            },
            // bloodletting for energy, energy drainer
            Unit {
                name: format!("Luna"),
                sprite_index: 13,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(10),
                attack: UnitValue::full(0),
                energy: UnitValue::full(4),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::AllyWithMost(energy()),
                            vec![num(imm(1), energy())],
                        )],
                        costs: vec![num(imm(4), life())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(energy()),
                            vec![num(imm(-2), energy())],
                        )],
                        costs: vec![num(imm(4), energy())],
                        trigger: None,
                    },
                ],
            },
            // attack on enemy targeted, gives back to the team
            Unit {
                name: format!("Thelonius"),
                sprite_index: 10,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(4),
                attack: UnitValue::full(0),
                energy: UnitValue::full(0),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::This, vec![num(imm(1), attack())])],
                        costs: vec![],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllEnemy,
                            watch: CombatTriggerWatch::Targeted,
                        }),
                    },
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AllOtherAlly, vec![num(imm(1), attack())])],
                        costs: vec![num(imm(3), attack())],
                        trigger: None,
                    },
                ],
            },
            // bloodletting for energy drain, ultimate
            Unit {
                name: format!("Abad"),
                sprite_index: 11,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(30),
                attack: UnitValue::full(0),
                energy: UnitValue::full(0),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::AbilitySource,
                            vec![num(imm(-2), energy())],
                        )],
                        costs: vec![num(imm(2), life())],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllAlly,
                            watch: CombatTriggerWatch::ValueDecrease(life()),
                        }),
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(life()),
                            vec![num(imm(-50), life())],
                        )],
                        costs: vec![num(imm(50), life())],
                        trigger: None,
                    },
                ],
            },
            // attack drain
            Unit {
                name: format!("Sylther"),
                sprite_index: 4,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(8),
                attack: UnitValue::full(0),
                energy: UnitValue::full(10),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(energy()),
                            vec![num(imm(-1), energy())],
                        )],
                        costs: vec![num(imm(1), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AllEnemy, vec![num(imm(-10), energy())])],
                        costs: vec![num(imm(30), life())],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::This,
                            watch: CombatTriggerWatch::ValueDecrease(life()),
                        }),
                    },
                ],
            },
            // joker
            Unit {
                name: format!("Joker"),
                sprite_index: 15,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(10),
                attack: UnitValue::full(0),
                energy: UnitValue::full(10),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::AbilitySource,
                            vec![num(imm(-1), energy())],
                        )],
                        costs: vec![num(imm(1), life())],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllOther,
                            watch: CombatTriggerWatch::ValueDecrease(life()),
                        }),
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::AbilitySource,
                            vec![num(imm(-1), energy())],
                        )],
                        costs: vec![num(imm(1), energy())],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllOther,
                            watch: CombatTriggerWatch::ValueDecrease(attack()),
                        }),
                    },
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AllOther, vec![num(imm(-2), energy())])],
                        costs: vec![num(imm(4), attack())],
                        trigger: None,
                    },
                ],
            },
            // Trion
            Unit {
                name: format!("Trion"),
                sprite_index: 12,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(1),
                attack: UnitValue::full(4),
                energy: UnitValue::full(15),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![fx(
                        CombatTarget::All,
                        vec![num(src_unit_neg(attack()), life())],
                    )],
                    costs: vec![num(imm(10), energy())],
                    trigger: None,
                }],
            },
            // Zener
            Unit {
                name: format!("Zener"),
                sprite_index: 9,
                level: 1,
                experience: 0,
                max_experience: 10,
                life: UnitValue::full(100),
                attack: UnitValue::full(6),
                energy: UnitValue::full(42),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::This, vec![num(imm(1), life())])],
                        costs: vec![num(imm(4), energy())],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::This,
                            watch: CombatTriggerWatch::ValueDecrease(life()),
                        }),
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::AbilitySource,
                            vec![num(src_unit_neg(attack()), life())],
                        )],
                        costs: vec![num(imm(20), energy())],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllOtherAlly,
                            watch: CombatTriggerWatch::ValueDecrease(life()),
                        }),
                    },
                ],
            },
        ];

        for unit in &units {
            for (i, ability) in unit.abilities.iter().enumerate() {
                let mut costs = String::new();
                for cost in &ability.costs {
                    costs += &(cost.describe() + " ");
                }
                println!(
                    "{:10}, L{}, A{}, E{}, for {}: {}",
                    if i == 0 { &unit.name } else { "" },
                    unit.life.current,
                    unit.attack.current,
                    unit.energy.current,
                    costs,
                    ability.describe()
                );
            }
        }

        Blueprints { units }
    }
}

pub fn level_up(rng: &mut ChaCha8Rng) -> Vec<CombatNumber> {
    let how_many = [1, 2, 3]
        .choose_weighted(rng, |i| (1. / (*i as f32).powi(2)))
        .expect("can't choose level up");

    let how_much_weights = [0., 0.1, 0.0001];

    (0..*how_many)
        .map(|_| {
            let how_much = [1, 2]
                .choose_weighted(rng, |i| how_much_weights[*i])
                .expect("can't choose level up amount");
            let values = [UnitValues::Attack, UnitValues::Life, UnitValues::Energy]
                .choose(rng)
                .expect("can't choose level up attribute");

            CombatNumber {
                source: CombatNumberSource::Immediate(*how_much as i32),
                values: values.clone(),
            }
        })
        .collect()
}

pub fn price(unit: &Unit) -> i32 {
    unit.level * 2 + 12 + unit.abilities.len() as i32 * 4
}
