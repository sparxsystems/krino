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
            Assert.AreEqual(0, Attributes.I.Lexeme.Verb.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Avalent));
            Assert.AreEqual(1, Attributes.I.Lexeme.Verb.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Monovalent));
            Assert.AreEqual(2, Attributes.I.Lexeme.Verb.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Bivalent));
            Assert.AreEqual(3, Attributes.I.Lexeme.Verb.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Trivalent));
            Assert.AreEqual(4, Attributes.I.Lexeme.Verb.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Quadrivalent));
            Assert.AreEqual(5, Attributes.I.Lexeme.Verb.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Pentavalent));

            BigInteger attributes = Attributes.I.Lexeme.Verb.Bivalent | Attributes.I.Lexeme.Verb.Unaccusative;
            int count = Attributes.I.Lexeme.Verb.GetNumberOfValencies(attributes);
            Assert.AreEqual(2, count);

            // If more valencies are encoded then the lowest one should be returned.
            attributes = Attributes.I.Lexeme.Verb.Monovalent | Attributes.I.Lexeme.Verb.Bivalent | Attributes.I.Lexeme.Verb.Unaccusative;
            count = Attributes.I.Lexeme.Verb.GetNumberOfValencies(attributes);
            Assert.AreEqual(1, count);


            // If the enum does not encode any valency then it should return 0. 
            count = Attributes.I.Lexeme.Verb.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Unaccusative);
            Assert.AreEqual(0, count);
        }

        [Test]
        public void IsValencySpecified()
        {
            Assert.IsTrue(Attributes.I.Lexeme.Verb.IsValencySpecified(Attributes.I.Lexeme.Verb.Avalent));
            Assert.IsTrue(Attributes.I.Lexeme.Verb.IsValencySpecified(Attributes.I.Lexeme.Verb.Monovalent));
            Assert.IsTrue(Attributes.I.Lexeme.Verb.IsValencySpecified(Attributes.I.Lexeme.Verb.Bivalent));
            Assert.IsTrue(Attributes.I.Lexeme.Verb.IsValencySpecified(Attributes.I.Lexeme.Verb.Trivalent));
            Assert.IsTrue(Attributes.I.Lexeme.Verb.IsValencySpecified(Attributes.I.Lexeme.Verb.Quadrivalent));
            Assert.IsTrue(Attributes.I.Lexeme.Verb.IsValencySpecified(Attributes.I.Lexeme.Verb.Pentavalent));

            Assert.IsTrue(Attributes.I.Lexeme.Verb.IsValencySpecified(Attributes.I.Lexeme.Verb.Bivalent | Attributes.I.Lexeme.Verb.Trivalent | Attributes.I.Lexeme.Verb.Unaccusative));

            Assert.IsFalse(Attributes.I.Lexeme.Verb.IsValencySpecified(Attributes.I.Lexeme.Verb.Unaccusative));
        }
    }
}
