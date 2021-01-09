using Krino.Domain.ConstructiveAdpositionalGrammar.AttributesArrangement;
using NUnit.Framework;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AttributesArrangement.Structural
{
    [TestFixture]
    public class VerbAttributesTest
    {
        [Test]
        public void GetNumberOfValencies()
        {
            Assert.AreEqual(0, Attributes.I.Lexeme.Verb.Valency.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Valency.Avalent));
            Assert.AreEqual(1, Attributes.I.Lexeme.Verb.Valency.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Valency.Monovalent));
            Assert.AreEqual(2, Attributes.I.Lexeme.Verb.Valency.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Valency.Bivalent));
            Assert.AreEqual(3, Attributes.I.Lexeme.Verb.Valency.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Valency.Trivalent));
            Assert.AreEqual(4, Attributes.I.Lexeme.Verb.Valency.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Valency.Quadrivalent));
            Assert.AreEqual(5, Attributes.I.Lexeme.Verb.Valency.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Valency.Pentavalent));

            BigInteger attributes = Attributes.I.Lexeme.Verb.Valency.Bivalent | Attributes.I.Lexeme.Verb.Unaccusative;
            int count = Attributes.I.Lexeme.Verb.Valency.GetNumberOfValencies(attributes);
            Assert.AreEqual(2, count);

            // If more valencies are encoded then the lowest one should be returned.
            attributes = Attributes.I.Lexeme.Verb.Valency.Monovalent | Attributes.I.Lexeme.Verb.Valency.Bivalent | Attributes.I.Lexeme.Verb.Unaccusative;
            count = Attributes.I.Lexeme.Verb.Valency.GetNumberOfValencies(attributes);
            Assert.AreEqual(1, count);


            // If the enum does not encode any valency then it should return -1. 
            count = Attributes.I.Lexeme.Verb.Valency.GetNumberOfValencies(Attributes.I.Lexeme.Verb.Unaccusative);
            Assert.AreEqual(-1, count);
        }

        [Test]
        public void IsValencySpecified()
        {
            Assert.IsTrue(Attributes.I.Lexeme.Verb.Valency.IsValencySpecified(Attributes.I.Lexeme.Verb.Valency.Avalent));
            Assert.IsTrue(Attributes.I.Lexeme.Verb.Valency.IsValencySpecified(Attributes.I.Lexeme.Verb.Valency.Monovalent));
            Assert.IsTrue(Attributes.I.Lexeme.Verb.Valency.IsValencySpecified(Attributes.I.Lexeme.Verb.Valency.Bivalent));
            Assert.IsTrue(Attributes.I.Lexeme.Verb.Valency.IsValencySpecified(Attributes.I.Lexeme.Verb.Valency.Trivalent));
            Assert.IsTrue(Attributes.I.Lexeme.Verb.Valency.IsValencySpecified(Attributes.I.Lexeme.Verb.Valency.Quadrivalent));
            Assert.IsTrue(Attributes.I.Lexeme.Verb.Valency.IsValencySpecified(Attributes.I.Lexeme.Verb.Valency.Pentavalent));

            Assert.IsTrue(Attributes.I.Lexeme.Verb.Valency.IsValencySpecified(Attributes.I.Lexeme.Verb.Valency.Bivalent | Attributes.I.Lexeme.Verb.Valency.Trivalent | Attributes.I.Lexeme.Verb.Unaccusative));

            Assert.IsFalse(Attributes.I.Lexeme.Verb.Valency.IsValencySpecified(Attributes.I.Lexeme.Verb.Unaccusative));
        }
    }
}
