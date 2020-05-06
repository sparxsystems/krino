using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using NUnit.Framework;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes.AttributesArrangement.Structural
{
    [TestFixture]
    public class VerbAttributesTest
    {
        [Test]
        public void GetNumberOfValencies()
        {
            Assert.AreEqual(0, Attributes.I.Verb.GetNumberOfValencies(Attributes.I.Verb.Avalent));
            Assert.AreEqual(1, Attributes.I.Verb.GetNumberOfValencies(Attributes.I.Verb.Monovalent));
            Assert.AreEqual(2, Attributes.I.Verb.GetNumberOfValencies(Attributes.I.Verb.Bivalent));
            Assert.AreEqual(3, Attributes.I.Verb.GetNumberOfValencies(Attributes.I.Verb.Trivalent));
            Assert.AreEqual(4, Attributes.I.Verb.GetNumberOfValencies(Attributes.I.Verb.Quadrivalent));
            Assert.AreEqual(5, Attributes.I.Verb.GetNumberOfValencies(Attributes.I.Verb.Pentavalent));

            BigInteger attributes = Attributes.I.Verb.Bivalent | Attributes.I.Verb.Unaccusative;
            int count = Attributes.I.Verb.GetNumberOfValencies(attributes);
            Assert.AreEqual(2, count);

            // If more valencies are encoded then the lowest one should be returned.
            attributes = Attributes.I.Verb.Monovalent | Attributes.I.Verb.Bivalent | Attributes.I.Verb.Unaccusative;
            count = Attributes.I.Verb.GetNumberOfValencies(attributes);
            Assert.AreEqual(1, count);


            // If the enum does not encode any valency then it should return 0. 
            count = Attributes.I.Verb.GetNumberOfValencies(Attributes.I.Verb.Unaccusative);
            Assert.AreEqual(0, count);
        }

        [Test]
        public void IsValencySpecified()
        {
            Assert.IsTrue(Attributes.I.Verb.IsValencySpecified(Attributes.I.Verb.Avalent));
            Assert.IsTrue(Attributes.I.Verb.IsValencySpecified(Attributes.I.Verb.Monovalent));
            Assert.IsTrue(Attributes.I.Verb.IsValencySpecified(Attributes.I.Verb.Bivalent));
            Assert.IsTrue(Attributes.I.Verb.IsValencySpecified(Attributes.I.Verb.Trivalent));
            Assert.IsTrue(Attributes.I.Verb.IsValencySpecified(Attributes.I.Verb.Quadrivalent));
            Assert.IsTrue(Attributes.I.Verb.IsValencySpecified(Attributes.I.Verb.Pentavalent));

            Assert.IsTrue(Attributes.I.Verb.IsValencySpecified(Attributes.I.Verb.Bivalent | Attributes.I.Verb.Trivalent | Attributes.I.Verb.Unaccusative));

            Assert.IsFalse(Attributes.I.Verb.IsValencySpecified(Attributes.I.Verb.Unaccusative));
        }
    }
}
