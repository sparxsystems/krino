using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes.StructuralAttributesArrangement
{
    [TestFixture]
    public class VerbAttributesTest
    {
        [Test]
        public void GetNumberOfValencies()
        {
            Assert.AreEqual(0, StructuralAttributes.I.Verb.GetNumberOfValencies(StructuralAttributes.I.Verb.Avalent));
            Assert.AreEqual(1, StructuralAttributes.I.Verb.GetNumberOfValencies(StructuralAttributes.I.Verb.Monovalent));
            Assert.AreEqual(2, StructuralAttributes.I.Verb.GetNumberOfValencies(StructuralAttributes.I.Verb.Bivalent));
            Assert.AreEqual(3, StructuralAttributes.I.Verb.GetNumberOfValencies(StructuralAttributes.I.Verb.Trivalent));
            Assert.AreEqual(4, StructuralAttributes.I.Verb.GetNumberOfValencies(StructuralAttributes.I.Verb.Quadrivalent));
            Assert.AreEqual(5, StructuralAttributes.I.Verb.GetNumberOfValencies(StructuralAttributes.I.Verb.Pentavalent));

            ulong attributes = StructuralAttributes.I.Verb.Bivalent | StructuralAttributes.I.Verb.Unaccusative;
            int count = StructuralAttributes.I.Verb.GetNumberOfValencies(attributes);
            Assert.AreEqual(2, count);

            // If more valencies are encoded then the lowest one should be returned.
            attributes = StructuralAttributes.I.Verb.Monovalent | StructuralAttributes.I.Verb.Bivalent | StructuralAttributes.I.Verb.Unaccusative;
            count = StructuralAttributes.I.Verb.GetNumberOfValencies(attributes);
            Assert.AreEqual(1, count);


            // If the enum does not encode any valency then it should return 0. 
            count = StructuralAttributes.I.Verb.GetNumberOfValencies(StructuralAttributes.I.Verb.Unaccusative);
            Assert.AreEqual(0, count);
        }

        [Test]
        public void IsValencySpecified()
        {
            Assert.IsTrue(StructuralAttributes.I.Verb.IsValencySpecified(StructuralAttributes.I.Verb.Avalent));
            Assert.IsTrue(StructuralAttributes.I.Verb.IsValencySpecified(StructuralAttributes.I.Verb.Monovalent));
            Assert.IsTrue(StructuralAttributes.I.Verb.IsValencySpecified(StructuralAttributes.I.Verb.Bivalent));
            Assert.IsTrue(StructuralAttributes.I.Verb.IsValencySpecified(StructuralAttributes.I.Verb.Trivalent));
            Assert.IsTrue(StructuralAttributes.I.Verb.IsValencySpecified(StructuralAttributes.I.Verb.Quadrivalent));
            Assert.IsTrue(StructuralAttributes.I.Verb.IsValencySpecified(StructuralAttributes.I.Verb.Pentavalent));

            Assert.IsTrue(StructuralAttributes.I.Verb.IsValencySpecified(StructuralAttributes.I.Verb.Bivalent | StructuralAttributes.I.Verb.Trivalent | StructuralAttributes.I.Verb.Unaccusative));

            Assert.IsFalse(StructuralAttributes.I.Verb.IsValencySpecified(StructuralAttributes.I.Verb.Unaccusative));
        }
    }
}
