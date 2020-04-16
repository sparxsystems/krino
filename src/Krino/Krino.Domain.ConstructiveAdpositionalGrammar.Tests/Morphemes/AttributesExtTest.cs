using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes
{
    [TestFixture]
    public class AttributesExtTest
    {
        [Test]
        public void ToValencyCount()
        {
            Assert.AreEqual(0, AttributeTypes.Avalent.ToValencyCount());
            Assert.AreEqual(1, AttributeTypes.Monovalent.ToValencyCount());
            Assert.AreEqual(2, AttributeTypes.Bivalent.ToValencyCount());
            Assert.AreEqual(3, AttributeTypes.Trivalent.ToValencyCount());
            Assert.AreEqual(4, AttributeTypes.Quadrivalent.ToValencyCount());
            Assert.AreEqual(5, AttributeTypes.Pentavalent.ToValencyCount());

            AttributeTypes attributes = AttributeTypes.Bivalent | AttributeTypes.Unaccusative;
            int count = attributes.ToValencyCount();
            Assert.AreEqual(2, count);

            // If more valencies are encoded then the lowest one should be returned.
            attributes = AttributeTypes.Monovalent | AttributeTypes.Bivalent | AttributeTypes.Unaccusative;
            count = attributes.ToValencyCount();
            Assert.AreEqual(1, count);


            // If the enum does not encode the valency then it should return 0. 
            Assert.AreEqual(0, AttributeTypes.Unaccusative.ToValencyCount());
        }

        [Test]
        public void IsValencySpecified()
        {
            Assert.IsTrue(AttributeTypes.Avalent.IsValencySpecified());
            Assert.IsTrue(AttributeTypes.Monovalent.IsValencySpecified());
            Assert.IsTrue(AttributeTypes.Bivalent.IsValencySpecified());
            Assert.IsTrue(AttributeTypes.Trivalent.IsValencySpecified());
            Assert.IsTrue(AttributeTypes.Quadrivalent.IsValencySpecified());
            Assert.IsTrue(AttributeTypes.Pentavalent.IsValencySpecified());

            Assert.IsTrue((AttributeTypes.Bivalent | AttributeTypes.Trivalent | AttributeTypes.PredicativeAdjective).IsValencySpecified());

            Assert.IsFalse(AttributeTypes.Unaccusative.IsValencySpecified());
        }
    }
}
