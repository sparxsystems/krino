using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes.StructuralAttributes
{
    [TestFixture]
    public class VerbTest
    {
        [Test]
        public void GetNumberOfValencies()
        {
            Assert.AreEqual(0, StructAttributes.Verb.GetNumberOfValencies(StructAttributes.Verb.Avalent));
            Assert.AreEqual(1, StructAttributes.Verb.GetNumberOfValencies(StructAttributes.Verb.Monovalent));
            Assert.AreEqual(2, StructAttributes.Verb.GetNumberOfValencies(StructAttributes.Verb.Bivalent));
            Assert.AreEqual(3, StructAttributes.Verb.GetNumberOfValencies(StructAttributes.Verb.Trivalent));
            Assert.AreEqual(4, StructAttributes.Verb.GetNumberOfValencies(StructAttributes.Verb.Quadrivalent));
            Assert.AreEqual(5, StructAttributes.Verb.GetNumberOfValencies(StructAttributes.Verb.Pentavalent));

            ulong attributes = StructAttributes.Verb.Bivalent | StructAttributes.Verb.Unaccusative;
            int count = StructAttributes.Verb.GetNumberOfValencies(attributes);
            Assert.AreEqual(2, count);

            // If more valencies are encoded then the lowest one should be returned.
            attributes = StructAttributes.Verb.Monovalent | StructAttributes.Verb.Bivalent | StructAttributes.Verb.Unaccusative;
            count = StructAttributes.Verb.GetNumberOfValencies(attributes);
            Assert.AreEqual(1, count);


            // If the enum does not encode any valency then it should return 0. 
            count = StructAttributes.Verb.GetNumberOfValencies(StructAttributes.Verb.Unaccusative);
            Assert.AreEqual(0, count);
        }

        [Test]
        public void IsValencySpecified()
        {
            Assert.IsTrue(StructAttributes.Verb.IsValencySpecified(StructAttributes.Verb.Avalent));
            Assert.IsTrue(StructAttributes.Verb.IsValencySpecified(StructAttributes.Verb.Monovalent));
            Assert.IsTrue(StructAttributes.Verb.IsValencySpecified(StructAttributes.Verb.Bivalent));
            Assert.IsTrue(StructAttributes.Verb.IsValencySpecified(StructAttributes.Verb.Trivalent));
            Assert.IsTrue(StructAttributes.Verb.IsValencySpecified(StructAttributes.Verb.Quadrivalent));
            Assert.IsTrue(StructAttributes.Verb.IsValencySpecified(StructAttributes.Verb.Pentavalent));

            Assert.IsTrue(StructAttributes.Verb.IsValencySpecified(StructAttributes.Verb.Bivalent | StructAttributes.Verb.Trivalent | StructAttributes.Verb.Unaccusative));

            Assert.IsFalse(StructAttributes.Verb.IsValencySpecified(StructAttributes.Verb.Unaccusative));
        }
    }
}
