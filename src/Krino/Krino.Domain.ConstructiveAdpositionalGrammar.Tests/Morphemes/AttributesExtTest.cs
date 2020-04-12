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
            Assert.AreEqual(0, Attributes.Avalent.ToValencyCount());
            Assert.AreEqual(1, Attributes.Monovalent.ToValencyCount());
            Assert.AreEqual(2, Attributes.Bivalent.ToValencyCount());
            Assert.AreEqual(3, Attributes.Trivalent.ToValencyCount());
            Assert.AreEqual(4, Attributes.Quadrivalent.ToValencyCount());
            Assert.AreEqual(5, Attributes.Pentavalent.ToValencyCount());

            Attributes attributes = Attributes.Bivalent | Attributes.Unaccusative;
            int count = attributes.ToValencyCount();
            Assert.AreEqual(2, count);

            // If more valencies are encoded then the lowest one should be returned.
            attributes = Attributes.Monovalent | Attributes.Bivalent | Attributes.Unaccusative;
            count = attributes.ToValencyCount();
            Assert.AreEqual(1, count);


            // If the enum does not encode the valency then it should return 0. 
            Assert.AreEqual(0, Attributes.Unaccusative.ToValencyCount());
        }

        [Test]
        public void IsValencySpecified()
        {
            Assert.IsTrue(Attributes.Avalent.IsValencySpecified());
            Assert.IsTrue(Attributes.Monovalent.IsValencySpecified());
            Assert.IsTrue(Attributes.Bivalent.IsValencySpecified());
            Assert.IsTrue(Attributes.Trivalent.IsValencySpecified());
            Assert.IsTrue(Attributes.Quadrivalent.IsValencySpecified());
            Assert.IsTrue(Attributes.Pentavalent.IsValencySpecified());

            Assert.IsTrue((Attributes.Bivalent | Attributes.Trivalent | Attributes.PredicativeAdjective).IsValencySpecified());

            Assert.IsFalse(Attributes.Unaccusative.IsValencySpecified());
        }
    }
}
