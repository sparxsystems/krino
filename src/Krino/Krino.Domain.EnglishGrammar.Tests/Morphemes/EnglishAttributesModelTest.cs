using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Numerics;
using System.Text;

namespace Krino.Domain.EnglishGrammar.Tests.Morphemes
{
    [TestFixture]
    public class EnglishAttributesModelTest
    {
        [Test]
        public void GetNumberOfValencies()
        {
            var attributesModel = new EnglishAttributesModel();

            Assert.AreEqual(0, attributesModel.GetNumberOfValencies(EnglishAttributes.I.Lexeme.Verb.Valency.Avalent));
            Assert.AreEqual(1, attributesModel.GetNumberOfValencies(EnglishAttributes.I.Lexeme.Verb.Valency.Monovalent));
            Assert.AreEqual(2, attributesModel.GetNumberOfValencies(EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent));
            Assert.AreEqual(3, attributesModel.GetNumberOfValencies(EnglishAttributes.I.Lexeme.Verb.Valency.Trivalent));
            Assert.AreEqual(4, attributesModel.GetNumberOfValencies(EnglishAttributes.I.Lexeme.Verb.Valency.Quadrivalent));
            Assert.AreEqual(5, attributesModel.GetNumberOfValencies(EnglishAttributes.I.Lexeme.Verb.Valency.Pentavalent));

            // If more valencies are encoded then the lowest one should be returned.
            BigInteger attributes = EnglishAttributes.I.Lexeme.Verb.Valency.Monovalent | EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent;
            int count = attributesModel.GetNumberOfValencies(attributes);
            Assert.AreEqual(1, count);


            // If the enum does not encode any valency then it should return -1. 
            count = attributesModel.GetNumberOfValencies(EnglishAttributes.I.Lexeme);
            Assert.AreEqual(-1, count);
        }

        [Test]
        public void IsValencySpecified()
        {
            var attributesModel = new EnglishAttributesModel();

            Assert.IsTrue(attributesModel.IsValencySpecified(EnglishAttributes.I.Lexeme.Verb.Valency.Avalent));
            Assert.IsTrue(attributesModel.IsValencySpecified(EnglishAttributes.I.Lexeme.Verb.Valency.Monovalent));
            Assert.IsTrue(attributesModel.IsValencySpecified(EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent));
            Assert.IsTrue(attributesModel.IsValencySpecified(EnglishAttributes.I.Lexeme.Verb.Valency.Trivalent));
            Assert.IsTrue(attributesModel.IsValencySpecified(EnglishAttributes.I.Lexeme.Verb.Valency.Quadrivalent));
            Assert.IsTrue(attributesModel.IsValencySpecified(EnglishAttributes.I.Lexeme.Verb.Valency.Pentavalent));

            Assert.IsTrue(attributesModel.IsValencySpecified(EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent | EnglishAttributes.I.Lexeme.Verb.Valency.Trivalent));

            Assert.IsFalse(attributesModel.IsValencySpecified(EnglishAttributes.I.Lexeme.Verb));
        }

        [Test]
        public void GetGrammarCharacter()
        {
            var attributesModel = new EnglishAttributesModel();

            Assert.AreEqual(GrammarCharacter.O, attributesModel.GetGrammarCharacter(EnglishAttributes.O));
            Assert.AreEqual(GrammarCharacter.I, attributesModel.GetGrammarCharacter(EnglishAttributes.I));
            Assert.AreEqual(GrammarCharacter.A, attributesModel.GetGrammarCharacter(EnglishAttributes.A));
            Assert.AreEqual(GrammarCharacter.E, attributesModel.GetGrammarCharacter(EnglishAttributes.E));
            Assert.AreEqual(GrammarCharacter.U, attributesModel.GetGrammarCharacter(EnglishAttributes.U));

            Assert.AreEqual(GrammarCharacter.e, attributesModel.GetGrammarCharacter(0));
        }
    }
}
