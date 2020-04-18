using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Attributes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes.Attributes
{
    [TestFixture]
    public class VerbTest
    {
        [Test]
        public void GetNumberOfValencies()
        {
            Assert.AreEqual(0, MorphemeAttributes.Verb.GetNumberOfValencies(MorphemeAttributes.Verb.Avalent));
            Assert.AreEqual(1, MorphemeAttributes.Verb.GetNumberOfValencies(MorphemeAttributes.Verb.Monovalent));
            Assert.AreEqual(2, MorphemeAttributes.Verb.GetNumberOfValencies(MorphemeAttributes.Verb.Bivalent));
            Assert.AreEqual(3, MorphemeAttributes.Verb.GetNumberOfValencies(MorphemeAttributes.Verb.Trivalent));
            Assert.AreEqual(4, MorphemeAttributes.Verb.GetNumberOfValencies(MorphemeAttributes.Verb.Quadrivalent));
            Assert.AreEqual(5, MorphemeAttributes.Verb.GetNumberOfValencies(MorphemeAttributes.Verb.Pentavalent));

            ulong attributes = MorphemeAttributes.Verb.Bivalent | MorphemeAttributes.Verb.Unaccusative;
            int count = MorphemeAttributes.Verb.GetNumberOfValencies(attributes);
            Assert.AreEqual(2, count);

            // If more valencies are encoded then the lowest one should be returned.
            attributes = MorphemeAttributes.Verb.Monovalent | MorphemeAttributes.Verb.Bivalent | MorphemeAttributes.Verb.Unaccusative;
            count = MorphemeAttributes.Verb.GetNumberOfValencies(attributes);
            Assert.AreEqual(1, count);


            // If the enum does not encode any valency then it should return 0. 
            count = MorphemeAttributes.Verb.GetNumberOfValencies(MorphemeAttributes.Verb.Unaccusative);
            Assert.AreEqual(0, count);
        }

        [Test]
        public void IsValencySpecified()
        {
            Assert.IsTrue(MorphemeAttributes.Verb.IsValencySpecified(MorphemeAttributes.Verb.Avalent));
            Assert.IsTrue(MorphemeAttributes.Verb.IsValencySpecified(MorphemeAttributes.Verb.Monovalent));
            Assert.IsTrue(MorphemeAttributes.Verb.IsValencySpecified(MorphemeAttributes.Verb.Bivalent));
            Assert.IsTrue(MorphemeAttributes.Verb.IsValencySpecified(MorphemeAttributes.Verb.Trivalent));
            Assert.IsTrue(MorphemeAttributes.Verb.IsValencySpecified(MorphemeAttributes.Verb.Quadrivalent));
            Assert.IsTrue(MorphemeAttributes.Verb.IsValencySpecified(MorphemeAttributes.Verb.Pentavalent));

            Assert.IsTrue(MorphemeAttributes.Verb.IsValencySpecified(MorphemeAttributes.Verb.Bivalent | MorphemeAttributes.Verb.Trivalent | MorphemeAttributes.Verb.Unaccusative));

            Assert.IsFalse(MorphemeAttributes.Verb.IsValencySpecified(MorphemeAttributes.Verb.Unaccusative));
        }
    }
}
