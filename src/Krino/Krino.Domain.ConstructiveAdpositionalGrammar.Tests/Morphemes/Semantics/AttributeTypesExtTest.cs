using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Semantics;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes
{
    [TestFixture]
    public class AttributeTypesExtTest
    {
        [Test]
        public void ToValencyCount()
        {
            Assert.AreEqual(0, VerbAttributeTypes.Avalent.ToValencyCount());
            Assert.AreEqual(1, VerbAttributeTypes.Monovalent.ToValencyCount());
            Assert.AreEqual(2, VerbAttributeTypes.Bivalent.ToValencyCount());
            Assert.AreEqual(3, VerbAttributeTypes.Trivalent.ToValencyCount());
            Assert.AreEqual(4, VerbAttributeTypes.Quadrivalent.ToValencyCount());
            Assert.AreEqual(5, VerbAttributeTypes.Pentavalent.ToValencyCount());

            VerbAttributeTypes attributes = VerbAttributeTypes.Bivalent | VerbAttributeTypes.Unaccusative;
            int count = attributes.ToValencyCount();
            Assert.AreEqual(2, count);

            // If more valencies are encoded then the lowest one should be returned.
            attributes = VerbAttributeTypes.Monovalent | VerbAttributeTypes.Bivalent | VerbAttributeTypes.Unaccusative;
            count = attributes.ToValencyCount();
            Assert.AreEqual(1, count);


            // If the enum does not encode the valency then it should return 0. 
            Assert.AreEqual(0, VerbAttributeTypes.Unaccusative.ToValencyCount());
        }

        [Test]
        public void IsValencySpecified()
        {
            Assert.IsTrue(VerbAttributeTypes.Avalent.IsValencySpecified());
            Assert.IsTrue(VerbAttributeTypes.Monovalent.IsValencySpecified());
            Assert.IsTrue(VerbAttributeTypes.Bivalent.IsValencySpecified());
            Assert.IsTrue(VerbAttributeTypes.Trivalent.IsValencySpecified());
            Assert.IsTrue(VerbAttributeTypes.Quadrivalent.IsValencySpecified());
            Assert.IsTrue(VerbAttributeTypes.Pentavalent.IsValencySpecified());

            Assert.IsTrue((VerbAttributeTypes.Bivalent | VerbAttributeTypes.Trivalent | VerbAttributeTypes.Unaccusative).IsValencySpecified());

            Assert.IsFalse(VerbAttributeTypes.Unaccusative.IsValencySpecified());
        }
    }
}
