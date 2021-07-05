using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticStructures
{
    [TestFixture]
    public class SentenceTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void SimpleSentence()
        {
            var adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.O1_I.SetLeftFirst())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb), EnglishPattern.I_Lexeme_Verb),
                Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme_Pronoun),
            };

            var factory = new LinguisticStructureFactory(myAttributesModel);
            var sentence = factory.CreateSentence(adTree);

            Assert.AreEqual((BigInteger)StructureAttributes.Sentence.SimpleSentence, sentence.Attributes);

            var clauses = sentence.Clauses.ToList();

            Assert.AreEqual(1, clauses.Count);
            Assert.AreEqual("I read", clauses[0].Value);

            Assert.AreEqual("I read", sentence.Value);
        }

        [Test]
        public void CompoundSentence()
        {
            var adTree = new AdTree(new Morpheme(myAttributesModel, ".", EnglishAttributes.U.NonLexeme.PunctuationMark), EnglishPattern.e_Period_I)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "and", EnglishAttributes.U.Lexeme.Conjunction.Coordinating), EnglishPattern.I_U_I)
                {
                    Right = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O1_I.SetLeftFirst())
                    {
                        Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb), EnglishPattern.I_Lexeme_Verb),
                        Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.O_Lexeme_Pronoun)
                    },
                    Left = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O1_I.SetLeftFirst())
                    {
                        Right = new AdTree(new Morpheme(myAttributesModel, "learn", EnglishAttributes.I.Lexeme.Verb), EnglishPattern.I_Lexeme_Verb),
                        Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.O_Lexeme_Pronoun)
                    }
                }
            };


            var factory = new LinguisticStructureFactory(myAttributesModel);
            var sentence = factory.CreateSentence(adTree);

            Assert.AreEqual((BigInteger)StructureAttributes.Sentence.CompoundSentence, sentence.Attributes);

            var clauses = sentence.Clauses.ToList();

            Assert.AreEqual(2, clauses.Count);
            Assert.AreEqual("I read", clauses[0].Value);
            Assert.AreEqual((BigInteger)StructureAttributes.Clause.Independent, clauses[0].Attributes);

            Assert.AreEqual("and I learn", clauses[1].Value);
            Assert.AreEqual((BigInteger)StructureAttributes.Clause.Independent, clauses[1].Attributes);
        }

        [Test]
        public void ComplexSentence_Argument()
        {
            var adTree = new AdTree(new Morpheme(myAttributesModel, ".", EnglishAttributes.U.NonLexeme.PunctuationMark), EnglishPattern.e_Period_I)
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "because", EnglishAttributes.U.Lexeme.Conjunction.Subordinating.Sememe.Cause), EnglishPattern.I_U_I)
                {
                    Right = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O1_I.SetLeftFirst())
                    {
                        Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb), EnglishPattern.I_Lexeme_Verb),
                        Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.O_Lexeme_Pronoun)
                    },
                    Left = new AdTree(Morpheme.Epsilon(myAttributesModel), EnglishPattern.O1_I.SetLeftFirst())
                    {
                        Right = new AdTree(new Morpheme(myAttributesModel, "learn", EnglishAttributes.I.Lexeme.Verb), EnglishPattern.I_Lexeme_Verb),
                        Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O), EnglishPattern.O_Lexeme_Pronoun)
                    }
                }
            };


            var factory = new LinguisticStructureFactory(myAttributesModel);
            var sentence = factory.CreateSentence(adTree);

            Assert.AreEqual((BigInteger)StructureAttributes.Sentence.ComplexSentence, sentence.Attributes);

            var clauses = sentence.Clauses.ToList();

            Assert.AreEqual(2, clauses.Count);
            Assert.AreEqual("I read", clauses[0].Value);
            Assert.IsTrue(StructureAttributes.Clause.Independent.IsIn(clauses[0].Attributes));
            Assert.IsTrue(StructureAttributes.Clause.Conclusion.IsIn(clauses[0].Attributes));

            Assert.AreEqual("because I learn", clauses[1].Value);
            Assert.IsTrue(StructureAttributes.Clause.Dependent.IsIn(clauses[1].Attributes));
            Assert.IsTrue(StructureAttributes.Clause.Premis.IsIn(clauses[1].Attributes));
        }
    }
}
