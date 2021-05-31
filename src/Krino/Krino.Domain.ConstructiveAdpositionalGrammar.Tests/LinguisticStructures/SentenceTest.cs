using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticStructures
{
    [TestFixture]
    public class SentenceTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void Clauses_SimpleSentence()
        {
            var adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.O1_I.SetLeftFirst())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb), EnglishPattern.Morpheme(EnglishAttributes.I)),
                Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O)),
            };

            var factory = new LinguisticStructureFactory(myAttributesModel);
            var sentence = factory.CreateSentence(adTree, 0);

            var clauses = sentence.Clauses.ToList();

            Assert.AreEqual(1, clauses.Count);
            Assert.AreEqual("I read", clauses[0].Value);

            Assert.AreEqual("I read", sentence.Value);
        }

        [Test]
        public void Clauses_ComplexSentence()
        {
            var adTree = new AdTree(new Morpheme(myAttributesModel, ".", EnglishAttributes.U.NonLexeme.PunctuationMark), EnglishPattern.MorphematicAdPosition(".", "", EnglishAttributes.U.NonLexeme.PunctuationMark, EnglishAttributes.I.Lexeme, EnglishAttributes.I.Lexeme))
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "and", EnglishAttributes.U.Lexeme.Conjunction.Coordinating), EnglishPattern.MorphematicAdPosition("I-U-I", "", EnglishAttributes.U.Lexeme.Conjunction.Coordinating, EnglishAttributes.I.Lexeme, EnglishAttributes.I.Lexeme))
                {
                    Right = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O1_I.SetLeftFirst())
                    {
                        Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb), EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme)),
                        Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme))
                    },
                    Left = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O1_I.SetLeftFirst())
                    {
                        Right = new AdTree(new Morpheme(myAttributesModel, "learn", EnglishAttributes.I.Lexeme.Verb), EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme)),
                        Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme))
                    }
                }
            };


            var factory = new LinguisticStructureFactory(myAttributesModel);
            var sentence = factory.CreateSentence(adTree, 0);

            var clauses = sentence.Clauses.ToList();

            Assert.AreEqual(2, clauses.Count);
            Assert.AreEqual("I read", clauses[0].Value);

            Assert.AreEqual("I learn", clauses[1].Value);
        }
    }
}
