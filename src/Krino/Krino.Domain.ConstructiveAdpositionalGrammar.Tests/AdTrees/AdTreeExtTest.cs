﻿using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeExtTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void GetSignature()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.O2_I)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.O1_I.SetLeftFirst())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I), EnglishPattern.I_Lexeme),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.O_Lexeme),
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.A_O)
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O), EnglishPattern.O_Lexeme),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A), EnglishPattern.A_Lexeme)
                }
            };

            Assert.AreEqual("OIAO", adTree.GetSignature());
        }

        [Test]
        public void GetSignature_NotFilled()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.O2_I)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.O1_I.SetLeftFirst())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I), EnglishPattern.I_Lexeme),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.O_Lexeme),
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.A_O)
                {
                    // Note: O is not filled yet. The signature shall not contain it.
                    Right = null,
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A), EnglishPattern.A_Lexeme)
                }
            };

            Assert.AreEqual("OIA", adTree.GetSignature());
        }

        [Test]
        public void GetSequenceToRoot()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", 0), new Pattern())
                }
            };

            List<IAdTree> sequence = adTree.Right.Left.GetSequenceToRoot().ToList();
            Assert.AreEqual(3, sequence.Count);
            Assert.IsTrue(adTree.Right.Left == sequence[0]);
            Assert.IsTrue(adTree.Right == sequence[1]);
            Assert.IsTrue(adTree == sequence[2]);
        }

        [Test]
        public void GetPath()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", 0), new Pattern())
                }
            };

            AdTreePosition[] path = adTree.Right.Left.GetPath();
            Assert.AreEqual(2, path.Length);
            Assert.AreEqual(AdTreePosition.ChildOnRight, path[0]);
            Assert.AreEqual(AdTreePosition.ChildOnLeft, path[1]);

            // Root
            path = adTree.GetPath();
            Assert.AreEqual(0, path.Length);
        }

        [Test]
        public void TryGetAdTree()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", 0), new Pattern())
                }
            };

            adTree.TryGetAdTree(new AdTreePosition[] { AdTreePosition.ChildOnRight, AdTreePosition.ChildOnLeft }, out IAdTree result);
            Assert.AreEqual("I", result.Morpheme.Morph);

            // Root
            adTree.TryGetAdTree(new AdTreePosition[] { }, out IAdTree root);
            Assert.IsTrue(adTree == root);
        }

        [Test]
        public void TryGetTransferenceMorpheme_MonoTransference()
        {
            AdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme),

                // Mono-transference.
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme), EnglishPattern.O_to_A)
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "race", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme),
                    Left = null,
                },
            };

            Assert.IsTrue(adTree.Left.Pattern.IsMonoTransference);

            var result = adTree.Left.TryGetTransferenceMorpheme();

            // Attributes shall be taken from the parent.
            Assert.AreEqual(adTree.Left.Morpheme.Attributes, result.Attributes);

            // Morph shall be taken from the child.
            Assert.AreEqual(adTree.Left.Right.Morpheme.Morph, result.Morph);
        }

        [Test]
        public void TryGetTransferenceMorpheme_PairTransference()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.O.Lexeme),
                EnglishPattern.I_to_O_ing)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme), EnglishPattern.I_Lexeme),
                Left = new AdTree(new Morpheme(myAttributesModel, "ing", EnglishAttributes.I.NonLexeme.Suffix), EnglishPattern.I_Suffix),
            };

            Assert.IsTrue(adTree.Pattern.IsPairTransference);

            var result = adTree.TryGetTransferenceMorpheme();

            // Attributes shall be taken from the parent.
            Assert.AreEqual(adTree.Morpheme.Attributes, result.Attributes);

            // Morph shall be composed from the left and right branches.
            Assert.AreEqual("reading", result.Morph);
        }

        [Test]
        public void CanAttachToRight()
        {
            IAdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { RightRule = EnglishMorphemeRule.O_Lexeme_Something, }
            );
            IAdTree adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                new Pattern() { UpRule = EnglishMorphemeRule.O_Lexeme_Something });
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement, myAttributesModel));


            // Primitive transference.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { RightRule = EnglishMorphemeRule.A_Lexeme_Something, }
            );
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme),
                EnglishPattern.O_to_A)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                    new Pattern() { UpRule = EnglishMorphemeRule.O_Lexeme_Something })
            };
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement, myAttributesModel));


            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { RightRule = EnglishMorphemeRule.O_Lexeme_Something, }
            );
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme),
                new Pattern() { UpRule = EnglishMorphemeRule.A_Lexeme_Something });
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement, myAttributesModel));
        }

        [Test]
        public void CanAttachToRight_Substitution()
        {
            IAdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { RightRule = EnglishMorphemeRule.O_Lexeme_Something, }
            );
            IAdTree adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { UpRule = MorphemeRule.Epsilon, LeftRule = EnglishMorphemeRule.A_Lexeme_Something, RightRule = EnglishMorphemeRule.O_Lexeme_Something })
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                    new Pattern() { UpRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme) }),
            };
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement, myAttributesModel));


            // Substitution is not allowed.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { RightRule = EnglishMorphemeRule.O_Lexeme_Something, }.SetMorphematicAdPositionRuleForRight(MorphematicAdPositionRules.Nothing)
            );
            adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { UpRule = MorphemeRule.Epsilon, LeftRule = EnglishMorphemeRule.A_Lexeme_Something, RightRule = EnglishMorphemeRule.O_Lexeme_Something })
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                    new Pattern() { UpRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme) }),
            };
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement, myAttributesModel));
        }

        [Test]
        public void CanAttachToRight_Substitution_Morphemic()
        {
            IAdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), Pattern.EpsilonAdPosition(myAttributesModel, "O-A", "", EnglishAttributes.O.Lexeme, EnglishAttributes.A.Lexeme))
            {
                Left = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O)
            };

            IAdTree adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme);
            Assert.IsTrue(adTree.Left.CanAttachToRight(adTreeElement, myAttributesModel));


            // The morpheme is not attached to the right yet - so only rules are evaluated.
            adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O);
            Assert.IsTrue(adTree.Left.CanAttachToRight(adTreeElement, myAttributesModel));

            // The morpheme is not attached to the right yet - so only rules are evaluated => incorrect rules.
            adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O);
            Assert.IsFalse(adTree.Left.CanAttachToRight(adTreeElement, myAttributesModel));


            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O)
            {
                Left = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O)
                {
                    Right = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O)
                }
            };

            // Now try to attach the morpheme.
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme);
            Assert.IsTrue(adTree.Left.Right.CanAttachToRight(adTreeElement, myAttributesModel));


            // Attach to the right on the root.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O);
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme);
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement, myAttributesModel));

            // Attach to the right on the root - incorrect morpheme.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O);
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme), EnglishPattern.A_Lexeme);
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement, myAttributesModel));
        }

        [Test]
        public void CanAttachToRight_ValencyPosition()
        {
            // The phrase: I read
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.O1_I)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent), EnglishPattern.I_Lexeme),
                Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme)
            };

            // Try to connect the second valency position.
            AdTree valency2 = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O2_I);
            Assert.IsTrue(valency2.CanAttachToRight(adTree, myAttributesModel));

            // Try to connect the first valency position.
            AdTree valency1 = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O1_I);
            Assert.IsFalse(valency1.CanAttachToRight(adTree, myAttributesModel));

            // Try to connect the third valency position.
            AdTree valency3 = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O3_I);
            Assert.IsFalse(valency3.CanAttachToRight(adTree, myAttributesModel));


            // Try to connect the verb directly to the second valency.
            adTree = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent), EnglishPattern.I_Lexeme);
            Assert.IsFalse(valency2.CanAttachToRight(adTree, myAttributesModel));
        }


        [Test]
        public void CanAttachToLeft()
        {
            IAdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { LeftRule = EnglishMorphemeRule.O_Lexeme_Something, }
            );
            IAdTree adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                new Pattern() { UpRule = EnglishMorphemeRule.O_Lexeme_Something });
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));


            // Primitive transference.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { LeftRule = EnglishMorphemeRule.A_Lexeme_Something, }
            );
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme),
                EnglishPattern.O_to_A)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                    new Pattern() { UpRule = EnglishMorphemeRule.O_Lexeme_Something })
            };
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));


            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { LeftRule = EnglishMorphemeRule.O_Lexeme_Something, }
            );
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme),
                new Pattern() { UpRule = EnglishMorphemeRule.A_Lexeme_Something });
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));
        }

        [Test]
        public void CanAttachToLeft_MorphemicAdPosition()
        {
            IAdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.U.Lexeme.Conjunction),
                 EnglishPattern.O_U_O
            );
            IAdTree adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), new Pattern() { UpRule = EnglishMorphemeRule.O_Lexeme_Something });
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));


            adTree = new AdTree(new Morpheme(myAttributesModel, "and", EnglishAttributes.U.Lexeme.Conjunction),
                 EnglishPattern.O_U_O
            )
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), new Pattern() { UpRule = EnglishMorphemeRule.O_Lexeme_Something })
            };
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "bike", EnglishAttributes.O.Lexeme), new Pattern() { UpRule = EnglishMorphemeRule.O_Lexeme_Something });
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));


            // Note: the adposition morpheme is not set therefore it should not be allowsed to attach the second child.
            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.U.Lexeme.Conjunction),
                 EnglishPattern.O_U_O
            )
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), new Pattern() { UpRule = EnglishMorphemeRule.O_Lexeme_Something })
            };
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "bike", EnglishAttributes.O.Lexeme), new Pattern() { UpRule = EnglishMorphemeRule.O_Lexeme_Something });
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));
        }

        [Test]
        public void CanAttachToLeft_Substitution()
        {
            IAdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { LeftRule = EnglishMorphemeRule.O_Lexeme_Something, }
            );
            IAdTree adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { UpRule = MorphemeRule.Epsilon, LeftRule = EnglishMorphemeRule.A_Lexeme_Something, RightRule = EnglishMorphemeRule.O_Lexeme_Something })
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                    new Pattern() { UpRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme) }),
            };
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));


            // Substitution is not allowed.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { LeftRule = EnglishMorphemeRule.O_Lexeme_Something, }.SetMorphematicAdPositionRuleForLeft(MorphematicAdPositionRules.Nothing)
            );
            adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { UpRule = MorphemeRule.Epsilon, LeftRule = EnglishMorphemeRule.A_Lexeme_Something, RightRule = EnglishMorphemeRule.O_Lexeme_Something })
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                    new Pattern() { UpRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme) }),
            };
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));
        }

        [Test]
        public void CanAttachToLeft_Substitution_Morphemic()
        {
            IAdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O)
            {
                Left = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O)
            };
            
            IAdTree adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme);
            Assert.IsTrue(adTree.Left.CanAttachToLeft(adTreeElement, myAttributesModel));


            // The morpheme is not attached to the right yet - so only rules are evaluated.
            adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O);
            Assert.IsTrue(adTree.Left.CanAttachToLeft(adTreeElement, myAttributesModel));


            // Attach to the left on the root.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O);
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme), EnglishPattern.A_Lexeme);
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));

            // Attach to the left on the root - incorrect morpheme.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O);
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme);
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));
        }


        [Test]
        public void GetFirstAdPositionOnLeft()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", 0), new Pattern())
                }
            };

            IAdTree result = adTree.Left.Right.GetFirstAdPositionOnLeft();
            Assert.IsTrue(adTree.Left == result);

            // It is already on left.
            result = adTree.Right.Left.GetFirstAdPositionOnLeft();
            Assert.IsTrue(adTree.Right.Left == result);

            // Root.
            result = adTree.GetFirstAdPositionOnLeft();
            Assert.IsNull(result);
        }

        [Test]
        public void Replace()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", 0), new Pattern())
                }
            };

            AdTree replace = new AdTree(new Morpheme(myAttributesModel, "hello", 0), new Pattern());

            adTree.Right.Replace(replace);

            Assert.IsTrue(adTree.Right == replace);
            Assert.IsTrue(adTree.Right.Right == replace.Right);
            Assert.IsTrue(adTree.Right.Left == replace.Left);
        }

        [Test]
        public void Attach()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern());
            AdTree toAppend = new AdTree(new Morpheme(myAttributesModel, "hello", 0), new Pattern());
            adTree.Attach(toAppend, AdTreePosition.ChildOnLeft);
            Assert.AreEqual("hello", adTree.Left.Morpheme.Morph);
            Assert.IsNull(adTree.Right);


            adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern());
            toAppend = new AdTree(new Morpheme(myAttributesModel, "hello", 0), new Pattern());
            adTree.Attach(toAppend, AdTreePosition.ChildOnRight);
            Assert.AreEqual("hello", adTree.Right.Morpheme.Morph);
            Assert.IsTrue(adTree.Left == null);
        }

        [Test]
        public void Insert()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "A1", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "A11", 0), new Pattern()),
            };
            AdTree toInsert = new AdTree(new Morpheme(myAttributesModel, "hello", 0), new Pattern());
            adTree.Right.Insert(toInsert, toInsert, AdTreePosition.ChildOnLeft);

            Assert.AreEqual("hello", adTree.Right.Morpheme.Morph);
            Assert.AreEqual("A11", adTree.Right.Left.Morpheme.Morph);


            adTree = new AdTree(new Morpheme(myAttributesModel, "A1", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "A11", 0), new Pattern()),
            };
            toInsert = new AdTree(new Morpheme(myAttributesModel, "hello", 0), new Pattern());
            adTree.Right.Insert(toInsert, toInsert, AdTreePosition.ChildOnRight);
            
            Assert.AreEqual("hello", adTree.Right.Morpheme.Morph);
            Assert.AreEqual("A11", adTree.Right.Right.Morpheme.Morph);


            // Inserting to the root.
            adTree = new AdTree(new Morpheme(myAttributesModel, "A1", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "A11", 0), new Pattern()),
            };
            toInsert = new AdTree(new Morpheme(myAttributesModel, "hello", 0), new Pattern());
            adTree.Insert(toInsert, toInsert, AdTreePosition.ChildOnRight);

            IAdTree root = adTree.Root;
            Assert.AreEqual("hello", root.Morpheme.Morph);
            Assert.AreEqual("A1", root.Right.Morpheme.Morph);
        }

        [Test]
        public void Detach()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "A1", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "A11", 0), new Pattern()),
            };

            adTree.Right.Detach();

            Assert.IsTrue(adTree.Right == null);
        }

        [Test]
        public void IsCorrect()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme);
            Assert.IsTrue(adTree.IsCorrect(myAttributesModel));

            // Empty string is not allowed.
            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme);
            Assert.IsFalse(adTree.IsCorrect(myAttributesModel));

            // Non-lexeme is not allowed.
            adTree = new AdTree(new Morpheme(myAttributesModel, "bla", EnglishAttributes.O.NonLexeme), EnglishPattern.O_Lexeme);
            Assert.IsFalse(adTree.IsCorrect(myAttributesModel));

            // Left and right rules are anything so it should also accept if they are null.
            adTree = new AdTree(new Morpheme(myAttributesModel, ".", EnglishAttributes.U.NonLexeme), new Pattern("")
            {
                UpRule = EnglishMorphemeRule.U_NonLexeme_Something,
                RightRule = MorphemeRule.Anything,
                LeftRule = MorphemeRule.Anything,
            });
            Assert.IsTrue(adTree.IsCorrect(myAttributesModel));
        }

        [Test]
        public void GetNonconformities_MorphemeRule()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme), EnglishPattern.A_Lexeme);
            List<IAdTree> nonconformities = adTree.GetNonconformities(myAttributesModel).ToList();
            Assert.AreEqual(0, nonconformities.Count);

            // Empty string does not match the rule.
            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme), EnglishPattern.A_Lexeme);
            nonconformities = adTree.GetNonconformities(myAttributesModel).ToList();
            Assert.AreEqual(1, nonconformities.Count);

            // Morpheme attributes does not match the rule.
            adTree = new AdTree(new Morpheme(myAttributesModel, "bla", EnglishAttributes.A.NonLexeme), EnglishPattern.A_Lexeme);
            nonconformities = adTree.GetNonconformities(myAttributesModel).ToList();
            Assert.AreEqual(1, nonconformities.Count);

            // Attached right children violates the rule.
            adTree = new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme), EnglishPattern.A_Lexeme)
            {
                // Note: the rule is nothing so having this attached violates the rule.
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            };

            nonconformities = adTree.GetNonconformities(myAttributesModel).ToList();
            Assert.AreEqual(1, nonconformities.Count);
        }

        [Test]
        public void GetNonconformities_MonoTransference()
        {
            AdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme),
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme), EnglishPattern.O_to_A)
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "race", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme),
                    Left = null,
                },
            };

            List<IAdTree> nonconformities = adTree.GetNonconformities(myAttributesModel).ToList();
            Assert.AreEqual(0, nonconformities.Count);
        }

        [Test]
        public void IsComplete()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme);
            Assert.IsTrue(adTree.IsComplete());

            // Missing morph.
            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme);
            Assert.IsFalse(adTree.IsComplete());

            // Missing attributes.
            adTree = new AdTree(new Morpheme(myAttributesModel, "book", 0), EnglishPattern.O_Lexeme);
            Assert.IsFalse(adTree.IsComplete());


            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme), EnglishPattern.O_to_A)
            {
                Left = null,
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
            };
            Assert.IsTrue(adTree.IsComplete());

            // Right is missing.
            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme), EnglishPattern.O_to_A)
            {
                Left = null,
                Right = null,
            };
            Assert.IsFalse(adTree.IsComplete());


            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O)
            {
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
            };
            Assert.IsTrue(adTree.IsComplete());

            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O)
            {
                Left = null,
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
            };
            Assert.IsFalse(adTree.IsComplete());

            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.A_O)
            {
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
                Right = null,
            };
            Assert.IsFalse(adTree.IsComplete());
        }

        [Test]
        public void MakeDeepCopy()
        {
            // The phrase: I read the book.
            var adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.U), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme), new Pattern())
                }
            };

            var copy = adTree.Right.MakeDeepCopy();
            Assert.IsNull(copy.AdPosition);
            Assert.IsTrue(adTree.Right.Equals(copy));
        }

        [Test]
        public void MakeShallowCopy()
        {
            // The phrase: I read the book.
            IAdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), new Pattern())
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.U), new Pattern())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme), new Pattern()),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme), new Pattern())
                }
            };

            // Note: the copy does not start from the root but the whole tree will be copied.
            IAdTree copy = adTree.Right.MakeShallowCopy();

            // The copy must be set to the same position as the original.
            Assert.IsTrue(adTree.Right.Morpheme == copy.Morpheme);
            Assert.IsTrue(adTree.Right.Pattern == copy.Pattern);

            // Get the root - for easier testing.
            adTree = adTree.Root;
            copy = copy.Root;

            Assert.IsTrue(adTree.Morpheme == copy.Morpheme);
            Assert.IsTrue(adTree.Pattern == copy.Pattern);

            Assert.IsTrue(adTree.Right.Morpheme == copy.Right.Morpheme);
            Assert.IsTrue(adTree.Right.Pattern == copy.Right.Pattern);

            Assert.IsTrue(adTree.Right.Right.Morpheme == copy.Right.Right.Morpheme);
            Assert.IsTrue(adTree.Right.Right.Pattern == copy.Right.Right.Pattern);

            Assert.IsTrue(adTree.Right.Left.Morpheme == copy.Right.Left.Morpheme);
            Assert.IsTrue(adTree.Right.Left.Pattern == copy.Right.Left.Pattern);

            Assert.IsTrue(adTree.Left.Morpheme == copy.Left.Morpheme);
            Assert.IsTrue(adTree.Left.Pattern == copy.Left.Pattern);

            Assert.IsTrue(adTree.Left.Right.Morpheme == copy.Left.Right.Morpheme);
            Assert.IsTrue(adTree.Left.Right.Pattern == copy.Left.Right.Pattern);

            Assert.IsTrue(adTree.Left.Left.Morpheme == copy.Left.Left.Morpheme);
            Assert.IsTrue(adTree.Left.Left.Pattern == copy.Left.Left.Pattern);
        }

        
    }
}
