using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticStructures
{
    [TestFixture]
    public class WordTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void Value()
        {
            var adTree = new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O), EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme));
            var word = new Word(adTree);
            Assert.AreEqual("book", word.Value);


            // AdTree for the word containing a suffix.
            adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.O.Lexeme),
                EnglishPattern.PairTransference("I>O_ing", "", EnglishAttributes.O.Lexeme, EnglishAttributes.I.NonLexeme.Suffix, EnglishAttributes.I.Lexeme.Verb))
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme), EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme.Verb)),
                Left = new AdTree(new Morpheme(myAttributesModel, "ing", EnglishAttributes.I.NonLexeme.Suffix), EnglishPattern.Morpheme(EnglishAttributes.I.NonLexeme.Suffix)),
            };

            word = new Word(adTree);
            Assert.AreEqual("reading", word.Value);
        }
    }
}
