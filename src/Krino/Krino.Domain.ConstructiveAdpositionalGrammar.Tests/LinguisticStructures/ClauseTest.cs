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
                    Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent), EnglishPattern.I_Lexeme),
                    Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme),
                },
                Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.A_O)
                {
                    Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme),
                    Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme), EnglishPattern.A_Lexeme)
                }
            };

            var factory = new LinguisticStructureFactory(myAttributesModel);
            var clause = factory.CreateClause(adTree, 0);

            Assert.AreEqual("I", clause.Subject.Value);
            Assert.AreEqual("read the book", clause.Predicate.Value);
        }

        [Test]
        public void SubjectAndPredicate_with_circumstantial()
        {
            var adTree = new AdTree(new Morpheme(myAttributesModel, ".", EnglishAttributes.U.NonLexeme.PunctuationMark.Period), EnglishPattern.I_U_I)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "in", EnglishAttributes.E.Lexeme.Preposition), EnglishPattern.O_E_I)
                {
                    Right = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O2_I)
                    {
                        Right = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O1_I.SetLeftFirst())
                        {
                            Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent), EnglishPattern.I_Lexeme),
                            Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme)
                        },
                        Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.A_O)
                        {
                            Right = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme),
                            Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme), EnglishPattern.A_Lexeme)
                        }
                    },
                    Left = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.A_O)
                    {
                        Right = new AdTree(new Morpheme(myAttributesModel, "room", EnglishAttributes.O), EnglishPattern.O_Lexeme),
                        Left = new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A), EnglishPattern.A_Lexeme)
                    }
                }
            };

            var factory = new LinguisticStructureFactory(myAttributesModel);
            var clause = factory.CreateClause(adTree, 0);

            Assert.AreEqual("I", clause.Subject.Value);
            Assert.AreEqual("read the book in the room", clause.Predicate.Value);
        }
    }
}
