using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeFactoryTest
    {
        private EnglishAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void PatternSignature()
        {
            var adTreeFactory = new AdTreeFactory(EnglishPattern.e_Period_I)
            {
                Right = new AdTreeFactory(EnglishPattern.O1_I.SetLeftFirst())
                {
                    Left = new AdTreeFactory(EnglishPattern.O_U_O)
                }
            };
            Assert.AreEqual("OUOIU", adTreeFactory.PatternSignature);

            adTreeFactory = new AdTreeFactory(EnglishPattern.O1_I.SetLeftFirst())
            {
                Right = new AdTreeFactory(EnglishPattern.Will_I)
            };
            Assert.AreEqual("OII", adTreeFactory.PatternSignature);
        }

        [Test]
        public void CreateAdTree()
        {
            var morphemeAdTrees = new IAdTree[]
            {
                new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme.Adjective), EnglishPattern.A_Lexeme_Adjective),
                new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun), EnglishPattern.O_Lexeme_Noun)
            };

            var adTreeFactory = new AdTreeFactory(EnglishPattern.A_O);
            var result = adTreeFactory.CreateAdTree(myAttributesModel, morphemeAdTrees);
            Assert.AreEqual("green", result.Left.Morpheme.Morph);
            Assert.AreEqual("book", result.Right.Morpheme.Morph);

            
            morphemeAdTrees = new IAdTree[]
            {
                new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner), EnglishPattern.A_Lexeme_Determiner),
                new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme.Adjective), EnglishPattern.A_Lexeme_Adjective),
                new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun), EnglishPattern.O_Lexeme_Noun)
            };

            adTreeFactory = new AdTreeFactory(EnglishPattern.A_O)
            {
                Right = new AdTreeFactory(EnglishPattern.A_O)
            };
            result = adTreeFactory.CreateAdTree(myAttributesModel, morphemeAdTrees);
            Assert.AreEqual("the", result.Left.Morpheme.Morph);
            Assert.AreEqual("green", result.Right.Left.Morpheme.Morph);
            Assert.AreEqual("book", result.Right.Right.Morpheme.Morph);


            morphemeAdTrees = new IAdTree[]
            {
                new AdTree(new Morpheme(myAttributesModel, "i", EnglishAttributes.O.Lexeme.Pronoun), EnglishPattern.O_Lexeme_Pronoun),
                new AdTree(new Morpheme(myAttributesModel, "and", EnglishAttributes.U.Lexeme.Conjunction), EnglishPattern.U_Lexeme_Conjunction),
                new AdTree(new Morpheme(myAttributesModel, "you", EnglishAttributes.O.Lexeme.Pronoun), EnglishPattern.O_Lexeme_Pronoun),
                new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb), EnglishPattern.I_Lexeme_Verb),
                new AdTree(new Morpheme(myAttributesModel, ".", EnglishAttributes.U.NonLexeme.PunctuationMark.Period), EnglishPattern.U_NonLexeme_Punctuation)
            };

            adTreeFactory = new AdTreeFactory(EnglishPattern.e_Period_I)
            {
                Right = new AdTreeFactory(EnglishPattern.O1_I.SetLeftFirst())
                {
                    Left = new AdTreeFactory(EnglishPattern.O_U_O)
                }
            };
            result = adTreeFactory.CreateAdTree(myAttributesModel, morphemeAdTrees);
            Assert.AreEqual("i", result.Right.Left.Right.Morpheme.Morph);
            Assert.AreEqual("and", result.Right.Left.Morpheme.Morph);
            Assert.AreEqual("you", result.Right.Left.Left.Morpheme.Morph);
            Assert.AreEqual("read", result.Right.Right.Morpheme.Morph);
            Assert.AreEqual(".", result.Morpheme.Morph);



            morphemeAdTrees = new IAdTree[]
            {
                new AdTree(new Morpheme(myAttributesModel, "i", EnglishAttributes.O.Lexeme.Pronoun), EnglishPattern.O_Lexeme_Pronoun),
                new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb), EnglishPattern.I_Lexeme_Verb),
                new AdTree(new Morpheme(myAttributesModel, "in", EnglishAttributes.E.Lexeme.Preposition), EnglishPattern.E_Lexeme_Preposition),
                new AdTree(new Morpheme(myAttributesModel, "room", EnglishAttributes.O.Lexeme.Noun), EnglishPattern.O_Lexeme_Noun)
            };

            adTreeFactory = new AdTreeFactory(EnglishPattern.O_E_I)
            {
                Right = new AdTreeFactory(EnglishPattern.O1_I.SetLeftFirst())
            };
            result = adTreeFactory.CreateAdTree(myAttributesModel, morphemeAdTrees);
            Assert.AreEqual("i", result.Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", result.Right.Right.Morpheme.Morph);
            Assert.AreEqual("in", result.Morpheme.Morph);
            Assert.AreEqual("room", result.Left.Morpheme.Morph);
        }

        [Test]
        public void CreateAdTree_NotEnoughWords()
        {
            var morphemeAdTrees = new IAdTree[]
            {
                new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner), EnglishPattern.A_Lexeme_Determiner),
                new AdTree(new Morpheme(myAttributesModel, "green", EnglishAttributes.A.Lexeme.Adjective), EnglishPattern.A_Lexeme_Adjective)
                
                // missing book here
            };

            var adTreeFactory = new AdTreeFactory(EnglishPattern.A_O)
            {
                Right = new AdTreeFactory(EnglishPattern.A_O)
            };
            var result = adTreeFactory.CreateAdTree(myAttributesModel, morphemeAdTrees);
            Assert.AreEqual("the", result.Left.Morpheme.Morph);
            Assert.AreEqual("green", result.Right.Left.Morpheme.Morph);
        }
    }
}
