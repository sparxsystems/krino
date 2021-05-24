using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticStructures
{
    [TestFixture]
    public class ClauseTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void SubjectAndPredicate()
        {
            var adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.O2_I)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.O1_I.SetLeftFirst())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent), EnglishPattern.Morpheme(EnglishAttributes.I)),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O)),
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.EpsilonAdPosition("", "", EnglishAttributes.A, EnglishAttributes.O).SetLeftFirst())
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O)),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.A))
                }
            };

            var factory = new LinguisticStructureFactory(myAttributesModel);
            var clause = factory.CreateClause(adTree);

            Assert.AreEqual("I", clause.Subject.Value);
            Assert.AreEqual("read the book", clause.Predicate.Value);
        }

        [Test]
        public void SubjectAndPredicate_with_circumstantial()
        {
            var adTree = new AdTree(new Morpheme(myAttributesModel, ".", EnglishAttributes.U.NonLexeme.PunctuationMark.Period), EnglishPattern.MorphematicAdPosition("I-U-I", "", EnglishAttributes.U.NonLexeme.PunctuationMark, EnglishAttributes.I.Lexeme, EnglishAttributes.I.Lexeme))
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "in", EnglishAttributes.E.Lexeme.Preposition), EnglishPattern.MorphematicAdPosition("O-E-I", "", EnglishAttributes.E.Lexeme, EnglishAttributes.O.Lexeme, EnglishAttributes.I.Lexeme))
                {
                    Right = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O2_I)
                    {
                        Right = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O1_I.SetLeftFirst())
                        {
                            Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent), EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme)),
                            Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme))
                        },
                        Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.EpsilonAdPosition("A-O", "", EnglishAttributes.A, EnglishAttributes.O).SetLeftFirst())
                        {
                            Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.O)),
                            Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.A))
                        }
                    },
                    Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.EpsilonAdPosition("A-O", "", EnglishAttributes.A, EnglishAttributes.O).SetLeftFirst())
                    {
                        Right = new AdTree(new Morpheme(myAttributesModel, "room", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme)),
                        Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A), EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme))
                    }
                }
            };

            var factory = new LinguisticStructureFactory(myAttributesModel);
            var clause = factory.CreateClause(adTree);

            Assert.AreEqual("I", clause.Subject.Value);
            Assert.AreEqual("read the book in the room", clause.Predicate.Value);
        }
    }
}
