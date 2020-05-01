using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeExtTest
    {
        [Test]
        public void GetPath()
        {
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { Attributes = StructuralAttributes.I.Verb } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { Attributes = StructuralAttributes.O.Pronoun } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { Attributes = StructuralAttributes.O.Noun } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { Attributes = StructuralAttributes.A.Determiner } }
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
                    Right = new AdTree() { Morpheme = new Morpheme("read") { Attributes = StructuralAttributes.I.Verb } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { Attributes = StructuralAttributes.O.Pronoun } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { Attributes = StructuralAttributes.O.Noun } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { Attributes = StructuralAttributes.A.Determiner } }
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
                Morpheme = new Morpheme("") { Attributes = StructuralAttributes.U },
                Pattern = new Pattern() { RightRule = new PatternRule(MorphemeRule.O), }
            };
            IAdTree adTreeElement = new AdTree()
            {
                Morpheme = new Morpheme("hello") { Attributes = StructuralAttributes.O },
                Pattern = new Pattern() { AdPositionRule = new PatternRule(MorphemeRule.U), }
            };
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement));


            adTree = new AdTree()
            {
                Morpheme = new Morpheme("") { Attributes = StructuralAttributes.U },
                // Stative (O) can connect to the right child.
                Pattern = new Pattern() { RightRule = new PatternRule(MorphemeRule.O), }
            };
            adTreeElement = new AdTree()
            {
                // But the element is the verbant (I)
                Morpheme = new Morpheme("hello") { Attributes = StructuralAttributes.I },
                Pattern = new Pattern() { AdPositionRule = new PatternRule(MorphemeRule.U), }
            };
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement));


            adTree = new AdTree()
            {
                // The adposition is circumstantial (E)
                Morpheme = new Morpheme("") { Attributes = StructuralAttributes.E },
                Pattern = new Pattern() { RightRule = new PatternRule(MorphemeRule.O), }
            };
            adTreeElement = new AdTree()
            {
                Morpheme = new Morpheme("hello") { Attributes = StructuralAttributes.O },

                // It is expected the adposition is the adposition (U).
                Pattern = new Pattern() { AdPositionRule = new PatternRule(MorphemeRule.U), }
            };
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
                    AdPositionRule = PatternRule.Anything,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
                },

                Right = new AdTree()
                {
                    Morpheme = new Morpheme("read") { Attributes = StructuralAttributes.I.Verb },
                    Pattern = new Pattern()
                    {
                        MorphemeRule = MorphemeRule.I2,
                        AdPositionRule = PatternRule.Anything,
                        RightRule = PatternRule.Nothing,
                        LeftRule = PatternRule.Nothing,
                    },
                },
                Left = new AdTree()
                {
                    Morpheme = new Morpheme("I") { Attributes = StructuralAttributes.O.Pronoun},
                    Pattern = new Pattern()
                    {
                        MorphemeRule = MorphemeRule.O,
                        AdPositionRule = PatternRule.Anything,
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
                    AdPositionRule = PatternRule.Anything,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
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
                    AdPositionRule = PatternRule.Anything,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
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
                    AdPositionRule = PatternRule.Anything,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
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
                Morpheme = new Morpheme("") { Attributes = StructuralAttributes.U },
                Pattern = new Pattern() { LeftRule = new PatternRule(MorphemeRule.O), }
            };
            IAdTree adTreeElement = new AdTree()
            {
                Morpheme = new Morpheme("hello") { Attributes = StructuralAttributes.O },
                Pattern = new Pattern() { AdPositionRule = new PatternRule(MorphemeRule.U), }
            };
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement));


            adTree = new AdTree()
            {
                Morpheme = new Morpheme("") { Attributes = StructuralAttributes.U },
                // Stative (O) can connect to the left child.
                Pattern = new Pattern() { LeftRule = new PatternRule(MorphemeRule.O), }
            };
            adTreeElement = new AdTree()
            {
                // But the element is the verbant (I)
                Morpheme = new Morpheme("hello") { Attributes = StructuralAttributes.I },
                Pattern = new Pattern() { AdPositionRule = new PatternRule(MorphemeRule.U), }
            };
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement));


            adTree = new AdTree()
            {
                // The adposition is circumstantial (E)
                Morpheme = new Morpheme("") { Attributes = StructuralAttributes.E },
                Pattern = new Pattern() { LeftRule = new PatternRule(MorphemeRule.O), }
            };
            adTreeElement = new AdTree()
            {
                Morpheme = new Morpheme("hello") { Attributes = StructuralAttributes.O },

                // It is expected the adposition is the adposition (U).
                Pattern = new Pattern() { AdPositionRule = new PatternRule(MorphemeRule.U), }
            };
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement));
        }
    }
}
