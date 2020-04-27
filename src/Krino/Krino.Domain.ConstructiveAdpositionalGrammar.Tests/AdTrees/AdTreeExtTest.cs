using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeExtTest
    {
        [Test]
        public void CanAttachToRight()
        {
            IAdTree adTree = new AdTree()
            {
                Morpheme = new Morpheme("") { Attributes = StructuralAttributes.U },
                Pattern = new Pattern() { RightRule = PatternRule.Anything.Where(MorphemeRule.O), }
            };
            IAdTree adTreeElement = new AdTree()
            {
                Morpheme = new Morpheme("hello") { Attributes = StructuralAttributes.O },
                Pattern = new Pattern() { AdPositionRule = PatternRule.Anything.Where(MorphemeRule.U), }
            };
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement));


            adTree = new AdTree()
            {
                Morpheme = new Morpheme("") { Attributes = StructuralAttributes.U },
                // Stative (O) can connect to the right child.
                Pattern = new Pattern() { RightRule = PatternRule.Anything.Where(MorphemeRule.O), }
            };
            adTreeElement = new AdTree()
            {
                // But the element is the verbant (I)
                Morpheme = new Morpheme("hello") { Attributes = StructuralAttributes.I },
                Pattern = new Pattern() { AdPositionRule = PatternRule.Anything.Where(MorphemeRule.U), }
            };
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement));


            adTree = new AdTree()
            {
                // The adposition is circumstantial (E)
                Morpheme = new Morpheme("") { Attributes = StructuralAttributes.E },
                Pattern = new Pattern() { RightRule = PatternRule.Anything.Where(MorphemeRule.O), }
            };
            adTreeElement = new AdTree()
            {
                Morpheme = new Morpheme("hello") { Attributes = StructuralAttributes.O },

                // It is expected the adposition is the adposition (U).
                Pattern = new Pattern() { AdPositionRule = PatternRule.Anything.Where(MorphemeRule.U), }
            };
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement));
        }


        [Test]
        public void CanAttachToLeft()
        {
            IAdTree adTree = new AdTree()
            {
                Morpheme = new Morpheme("") { Attributes = StructuralAttributes.U },
                Pattern = new Pattern() { LeftRule = PatternRule.Anything.Where(MorphemeRule.O), }
            };
            IAdTree adTreeElement = new AdTree()
            {
                Morpheme = new Morpheme("hello") { Attributes = StructuralAttributes.O },
                Pattern = new Pattern() { AdPositionRule = PatternRule.Anything.Where(MorphemeRule.U), }
            };
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement));


            adTree = new AdTree()
            {
                Morpheme = new Morpheme("") { Attributes = StructuralAttributes.U },
                // Stative (O) can connect to the left child.
                Pattern = new Pattern() { LeftRule = PatternRule.Anything.Where(MorphemeRule.O), }
            };
            adTreeElement = new AdTree()
            {
                // But the element is the verbant (I)
                Morpheme = new Morpheme("hello") { Attributes = StructuralAttributes.I },
                Pattern = new Pattern() { AdPositionRule = PatternRule.Anything.Where(MorphemeRule.U), }
            };
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement));


            adTree = new AdTree()
            {
                // The adposition is circumstantial (E)
                Morpheme = new Morpheme("") { Attributes = StructuralAttributes.E },
                Pattern = new Pattern() { LeftRule = PatternRule.Anything.Where(MorphemeRule.O), }
            };
            adTreeElement = new AdTree()
            {
                Morpheme = new Morpheme("hello") { Attributes = StructuralAttributes.O },

                // It is expected the adposition is the adposition (U).
                Pattern = new Pattern() { AdPositionRule = PatternRule.Anything.Where(MorphemeRule.U), }
            };
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement));
        }
    }
}
