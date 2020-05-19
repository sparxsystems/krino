﻿using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeExtTest
    {
        [Test]
        public void GetSequenceToRoot()
        {
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner } }
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
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner } }
                }
            };

            byte[] path = adTree.Right.Left.GetPath();
            Assert.AreEqual(2, path.Length);
            Assert.AreEqual(2, path[0]);
            Assert.AreEqual(1, path[1]);

            // Root
            path = adTree.GetPath();
            Assert.AreEqual(0, path.Length);
        }

        [Test]
        public void TryGetAdTree()
        {
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner } }
                }
            };

            adTree.TryGetAdTree(new byte[] { 2, 1 }, out IAdTree result);
            Assert.AreEqual("I", result.Morpheme.Morph);

            // Root
            adTree.TryGetAdTree(new byte[] { }, out IAdTree root);
            Assert.IsTrue(adTree == root);
        }

        [Test]
        public void CanAttachToRight()
        {
            IAdTree adTree = new AdTree()
            {
                Morpheme = new Morpheme("") { Attributes = Attributes.U },
                Pattern = new Pattern() { RightRule = new PatternRule(MorphemeRule.O_Lexeme), }
            };
            IAdTree adTreeElement = new AdTree(new Morpheme("hello") { Attributes = Attributes.O.Lexeme }, Pattern.O);
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement));


            adTree = new AdTree()
            {
                Morpheme = new Morpheme("") { Attributes = Attributes.U },
                // Stative (O) can connect to the right child.
                Pattern = new Pattern() { RightRule = new PatternRule(MorphemeRule.O_Lexeme), }
            };
            // But the element is the verbant (I)
            adTreeElement = new AdTree(new Morpheme("hello") { Attributes = Attributes.I.Lexeme }, Pattern.I);
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement));
        }

        [Test]
        public void CanAttachToRight_ValencyPosition()
        {
            // The phrase: I read
            AdTree adTree = new AdTree()
            {
                Pattern = new Pattern()
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I_Lexeme),
                    LeftRule = new PatternRule(MorphemeRule.O_Lexeme)
                },

                Right = new AdTree()
                {
                    Morpheme = new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb },
                    Pattern = new Pattern()
                    {
                        MorphemeRule = MorphemeRule.I2,
                        RightRule = PatternRule.Nothing,
                        LeftRule = PatternRule.Nothing,
                    },
                },
                Left = new AdTree()
                {
                    Morpheme = new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun},
                    Pattern = new Pattern()
                    {
                        MorphemeRule = MorphemeRule.O_Lexeme,
                        RightRule = PatternRule.Nothing,
                        LeftRule = PatternRule.Nothing,
                    },
                }
            };

            // Try to connect the second valency position.
            AdTree valency2 = new AdTree()
            {
                Pattern = new Pattern()
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.Second,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I_Lexeme),
                    LeftRule = new PatternRule(MorphemeRule.O_Lexeme)
                },
            };

            Assert.IsTrue(valency2.CanAttachToRight(adTree));


            // Try to connect the first valency position.
            AdTree valency1 = new AdTree()
            {
                Pattern = new Pattern()
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I_Lexeme),
                    LeftRule = new PatternRule(MorphemeRule.O_Lexeme)
                },
            };

            // The first valency is already there so it should be false.
            Assert.IsFalse(valency1.CanAttachToRight(adTree));

            // Try to connect the third valency position.
            AdTree valency3 = new AdTree()
            {
                Pattern = new Pattern()
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.Third,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I_Lexeme),
                    LeftRule = new PatternRule(MorphemeRule.O_Lexeme)
                },
            };

            // The second valency is missing so it should be false.
            Assert.IsFalse(valency3.CanAttachToRight(adTree));
        }


        [Test]
        public void CanAttachToLeft()
        {
            IAdTree adTree = new AdTree()
            {
                Morpheme = new Morpheme("") { Attributes = Attributes.U },
                Pattern = new Pattern() { LeftRule = new PatternRule(MorphemeRule.O_Lexeme), }
            };
            IAdTree adTreeElement = new AdTree(new Morpheme("hello") { Attributes = Attributes.O.Lexeme }, new Pattern());
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement));


            adTree = new AdTree()
            {
                Morpheme = new Morpheme("") { Attributes = Attributes.U },
                // Stative (O) can connect to the left child.
                Pattern = new Pattern() { LeftRule = new PatternRule(MorphemeRule.O_Lexeme), }
            };
            // But the element is the verbant (I)
            adTreeElement = new AdTree(new Morpheme("hello") { Attributes = Attributes.I.Lexeme }, new Pattern());
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement));
        }


        [Test]
        public void GetFirstAdPositionOnLeft()
        {
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner } }
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
    }
}
