using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
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
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I), EnglishPattern.Morpheme(EnglishAttributes.I)),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O)),
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.EpsilonAdPosition("", EnglishAttributes.A, EnglishAttributes.O).SetLeftFirst())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O)),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A), EnglishPattern.Morpheme(EnglishAttributes.A))
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
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I), EnglishPattern.Morpheme(EnglishAttributes.I)),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O)),
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.EpsilonAdPosition("", EnglishAttributes.A, EnglishAttributes.O)
                    .SetLeftFirst())
                {
                    // Note: O is not filled yet. The signature shall not contain it.
                    Right = null,
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A), EnglishPattern.Morpheme(EnglishAttributes.A))
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

            AttachingPosition[] path = adTree.Right.Left.GetPath();
            Assert.AreEqual(2, path.Length);
            Assert.AreEqual(AttachingPosition.ChildOnRight, path[0]);
            Assert.AreEqual(AttachingPosition.ChildOnLeft, path[1]);

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

            adTree.TryGetAdTree(new AttachingPosition[] { AttachingPosition.ChildOnRight, AttachingPosition.ChildOnLeft }, out IAdTree result);
            Assert.AreEqual("I", result.Morpheme.Morph);

            // Root
            adTree.TryGetAdTree(new AttachingPosition[] { }, out IAdTree root);
            Assert.IsTrue(adTree == root);
        }

        [Test]
        public void TryGetTransferenceMorpheme_MonoTransference()
        {
            AdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme))
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme)),

                // Mono-transference.
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme), EnglishPattern.MonoTransference("O>A", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme))
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "race", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme)),
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
                EnglishPattern.PairTransference("I>O_ing", EnglishAttributes.O.Lexeme, EnglishAttributes.I.NonLexeme.Suffix, EnglishAttributes.I.Lexeme.Verb))
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme.Verb)),
                Left = new AdTree(new Morpheme(myAttributesModel, "ing", EnglishAttributes.I.NonLexeme.Suffix), EnglishPattern.Morpheme(EnglishAttributes.I.NonLexeme.Suffix)),
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
                new Pattern() { RightRule = EnglishMorphemeRule.O_Lexeme, }
            );
            IAdTree adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                new Pattern() { MorphemeRule = EnglishMorphemeRule.O_Lexeme });
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement, myAttributesModel));


            // Primitive transference.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { RightRule = EnglishMorphemeRule.A_Lexeme, }
            );
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme),
                EnglishPattern.MonoTransference("O>A", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme))
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                    new Pattern() { MorphemeRule = EnglishMorphemeRule.O_Lexeme })
            };
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement, myAttributesModel));


            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { RightRule = EnglishMorphemeRule.O_Lexeme, }
            );
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme),
                new Pattern() { MorphemeRule = EnglishMorphemeRule.A_Lexeme });
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement, myAttributesModel));
        }

        [Test]
        public void CanAttachToRight_Inheritance()
        {
            IAdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { RightRule = EnglishMorphemeRule.O_Lexeme, }
            );
            IAdTree adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { MorphemeRule = MorphemeRule.Epsilon, LeftRule = EnglishMorphemeRule.A_Lexeme, RightRule = EnglishMorphemeRule.O_Lexeme })
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                    new Pattern() { MorphemeRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme) }),
            };
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement, myAttributesModel));


            // Inheritance is not allowed.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { RightRule = EnglishMorphemeRule.O_Lexeme, }.SetInheritanceForRight(InheritanceRules.Nothing)
            );
            adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { MorphemeRule = MorphemeRule.Epsilon, LeftRule = EnglishMorphemeRule.A_Lexeme, RightRule = EnglishMorphemeRule.O_Lexeme })
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                    new Pattern() { MorphemeRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme) }),
            };
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement, myAttributesModel));
        }

        [Test]
        public void CanAttachToRight_Inheritance_Morphemic()
        {
            IAdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("O-A", EnglishAttributes.O.Lexeme, EnglishAttributes.A.Lexeme))
            {
                Left = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme))
            };

            IAdTree adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme));
            Assert.IsTrue(adTree.Left.CanAttachToRight(adTreeElement, myAttributesModel));


            // The morpheme is not attached to the right yet - so only rules are evaluated.
            adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme));
            Assert.IsTrue(adTree.Left.CanAttachToRight(adTreeElement, myAttributesModel));

            // The morpheme is not attached to the right yet - so only rules are evaluated => incorrect rules.
            adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("O-A", EnglishAttributes.O.Lexeme, EnglishAttributes.A.Lexeme));
            Assert.IsFalse(adTree.Left.CanAttachToRight(adTreeElement, myAttributesModel));


            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("O-A", EnglishAttributes.O.Lexeme, EnglishAttributes.A.Lexeme))
            {
                Left = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme))
                {
                    Right = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme))
                }
            };

            // Now try to attach the morpheme.
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme));
            Assert.IsTrue(adTree.Left.Right.CanAttachToRight(adTreeElement, myAttributesModel));


            // Attach to the right on the root.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme));
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme));
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement, myAttributesModel));

            // Attach to the right on the root - incorrect morpheme.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme));
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme));
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement, myAttributesModel));
        }

        [Test]
        public void CanAttachToRight_ValencyPosition()
        {
            // The phrase: I read
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.O1_I)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent), EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent)),
                Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme))
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
            adTree = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent), EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent));
            Assert.IsFalse(valency2.CanAttachToRight(adTree, myAttributesModel));
        }


        [Test]
        public void CanAttachToLeft()
        {
            IAdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { LeftRule = EnglishMorphemeRule.O_Lexeme, }
            );
            IAdTree adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                new Pattern() { MorphemeRule = EnglishMorphemeRule.O_Lexeme });
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));


            // Primitive transference.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { LeftRule = EnglishMorphemeRule.A_Lexeme, }
            );
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme),
                EnglishPattern.MonoTransference("O>A", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme))
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                    new Pattern() { MorphemeRule = EnglishMorphemeRule.O_Lexeme })
            };
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));


            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { LeftRule = EnglishMorphemeRule.O_Lexeme, }
            );
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme),
                new Pattern() { MorphemeRule = EnglishMorphemeRule.A_Lexeme });
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));
        }

        [Test]
        public void CanAttachToLeft_MorphemicAdPosition()
        {
            IAdTree adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.U.Lexeme.Conjunction),
                 EnglishPattern.MorphematicAdPosition("O-U-O", EnglishAttributes.U.Lexeme.Conjunction, EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme).SetLeftFirst()
            );
            IAdTree adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), new Pattern() { MorphemeRule = EnglishMorphemeRule.O_Lexeme });
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));


            adTree = new AdTree(new Morpheme(myAttributesModel, "and", EnglishAttributes.U.Lexeme.Conjunction),
                 EnglishPattern.MorphematicAdPosition("O-U-O", EnglishAttributes.U.Lexeme.Conjunction, EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme)
            )
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), new Pattern() { MorphemeRule = EnglishMorphemeRule.O_Lexeme })
            };
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "bike", EnglishAttributes.O.Lexeme), new Pattern() { MorphemeRule = EnglishMorphemeRule.O_Lexeme });
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));


            // Note: the adposition morpheme is not set therefore it should not be allowsed to attach the second child.
            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.U.Lexeme.Conjunction),
                 EnglishPattern.MorphematicAdPosition("O-U-O", EnglishAttributes.U.Lexeme.Conjunction, EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme)
            )
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), new Pattern() { MorphemeRule = EnglishMorphemeRule.O_Lexeme })
            };
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "bike", EnglishAttributes.O.Lexeme), new Pattern() { MorphemeRule = EnglishMorphemeRule.O_Lexeme });
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));
        }

        [Test]
        public void CanAttachToLeft_Inheritance()
        {
            IAdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { LeftRule = EnglishMorphemeRule.O_Lexeme, }
            );
            IAdTree adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { MorphemeRule = MorphemeRule.Epsilon, LeftRule = EnglishMorphemeRule.A_Lexeme, RightRule = EnglishMorphemeRule.O_Lexeme })
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                    new Pattern() { MorphemeRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme) }),
            };
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));


            // Inheritance is not allowed.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { LeftRule = EnglishMorphemeRule.O_Lexeme, }.SetInheritanceForLeft(InheritanceRules.Nothing)
            );
            adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel),
                new Pattern() { MorphemeRule = MorphemeRule.Epsilon, LeftRule = EnglishMorphemeRule.A_Lexeme, RightRule = EnglishMorphemeRule.O_Lexeme })
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme),
                    new Pattern() { MorphemeRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme) }),
            };
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));
        }

        [Test]
        public void CanAttachToLeft_Inheritance_Morphemic()
        {
            IAdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme))
            {
                Left = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("O-A", EnglishAttributes.O.Lexeme, EnglishAttributes.A.Lexeme).SetLeftFirst())
            };
            
            IAdTree adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme));
            Assert.IsTrue(adTree.Left.CanAttachToLeft(adTreeElement, myAttributesModel));


            // The morpheme is not attached to the right yet - so only rules are evaluated.
            adTreeElement = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme));
            Assert.IsTrue(adTree.Left.CanAttachToLeft(adTreeElement, myAttributesModel));


            // Attach to the left on the root.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme).SetLeftFirst());
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme));
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement, myAttributesModel));

            // Attach to the left on the root - incorrect morpheme.
            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme));
            adTreeElement = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme));
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
            adTree.Attach(toAppend, AttachingPosition.ChildOnLeft);
            Assert.AreEqual("hello", adTree.Left.Morpheme.Morph);
            Assert.IsNull(adTree.Right);


            adTree = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern());
            toAppend = new AdTree(new Morpheme(myAttributesModel, "hello", 0), new Pattern());
            adTree.Attach(toAppend, AttachingPosition.ChildOnRight);
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
            adTree.Right.Insert(toInsert, toInsert, AttachingPosition.ChildOnLeft);

            Assert.AreEqual("hello", adTree.Right.Morpheme.Morph);
            Assert.AreEqual("A11", adTree.Right.Left.Morpheme.Morph);


            adTree = new AdTree(new Morpheme(myAttributesModel, "A1", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "A11", 0), new Pattern()),
            };
            toInsert = new AdTree(new Morpheme(myAttributesModel, "hello", 0), new Pattern());
            adTree.Right.Insert(toInsert, toInsert, AttachingPosition.ChildOnRight);
            
            Assert.AreEqual("hello", adTree.Right.Morpheme.Morph);
            Assert.AreEqual("A11", adTree.Right.Right.Morpheme.Morph);


            // Inserting to the root.
            adTree = new AdTree(new Morpheme(myAttributesModel, "A1", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "A11", 0), new Pattern()),
            };
            toInsert = new AdTree(new Morpheme(myAttributesModel, "hello", 0), new Pattern());
            adTree.Insert(toInsert, toInsert, AttachingPosition.ChildOnRight);

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
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme));
            Assert.IsTrue(adTree.IsCorrect(myAttributesModel));

            // Empty string is not allowed.
            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme));
            Assert.IsFalse(adTree.IsCorrect(myAttributesModel));

            // Non-lexeme is not allowed.
            adTree = new AdTree(new Morpheme(myAttributesModel, "bla", EnglishAttributes.O.NonLexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme));
            Assert.IsFalse(adTree.IsCorrect(myAttributesModel));

            // Left and right rules are anything so it should also accept if they are null.
            adTree = new AdTree(new Morpheme(myAttributesModel, ".", EnglishAttributes.U.NonLexeme), new Pattern("")
            {
                MorphemeRule = EnglishMorphemeRule.U_NonLexeme,
                RightRule = MorphemeRule.Anything,
                LeftRule = MorphemeRule.Anything,
            });
            Assert.IsTrue(adTree.IsCorrect(myAttributesModel));
        }

        [Test]
        public void GetNonconformities_MorphemeRule()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme));
            List<IAdTree> nonconformities = adTree.GetNonconformities(myAttributesModel).ToList();
            Assert.AreEqual(0, nonconformities.Count);

            // Empty string does not match the rule.
            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme));
            nonconformities = adTree.GetNonconformities(myAttributesModel).ToList();
            Assert.AreEqual(1, nonconformities.Count);

            // Morpheme attributes does not match the rule.
            adTree = new AdTree(new Morpheme(myAttributesModel, "bla", EnglishAttributes.A.NonLexeme), EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme));
            nonconformities = adTree.GetNonconformities(myAttributesModel).ToList();
            Assert.AreEqual(1, nonconformities.Count);

            // Attached right children violates the rule.
            adTree = new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme))
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
            AdTree adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme))
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "car", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme)),
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme), EnglishPattern.MonoTransference("O>A", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme))
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "race", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme)),
                    Left = null,
                },
            };

            List<IAdTree> nonconformities = adTree.GetNonconformities(myAttributesModel).ToList();
            Assert.AreEqual(0, nonconformities.Count);
        }

        [Test]
        public void IsComplete()
        {
            AdTree adTree = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme));
            Assert.IsTrue(adTree.IsComplete());

            // Missing morph.
            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme));
            Assert.IsFalse(adTree.IsComplete());

            // Missing attributes.
            adTree = new AdTree(new Morpheme(myAttributesModel, "book", 0), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme));
            Assert.IsFalse(adTree.IsComplete());


            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme), EnglishPattern.MonoTransference("O>A", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme))
            {
                Left = null,
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
            };
            Assert.IsTrue(adTree.IsComplete());

            // Right is missing.
            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.A.Lexeme), EnglishPattern.MonoTransference("O>A", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme))
            {
                Left = null,
                Right = null,
            };
            Assert.IsFalse(adTree.IsComplete());


            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A, EnglishAttributes.O))
            {
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
            };
            Assert.IsTrue(adTree.IsComplete());

            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A, EnglishAttributes.O))
            {
                Left = null,
                Right = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
            };
            Assert.IsFalse(adTree.IsComplete());

            adTree = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A, EnglishAttributes.O))
            {
                Left = new AdTree(new Morpheme(myAttributesModel, "", 0), new Pattern()),
                Right = null,
            };
            Assert.IsFalse(adTree.IsComplete());
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
