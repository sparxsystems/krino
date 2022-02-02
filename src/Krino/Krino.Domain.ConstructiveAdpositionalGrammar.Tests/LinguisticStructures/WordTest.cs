﻿using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
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
            //var adTree = new AdTree(new ConstructiveAdpositionalGrammar.Morphemes.Morpheme(myAttributesModel, "book", EnglishAttributes.O), EnglishPattern.O_Lexeme_Noun);
            //var word = new Word(EnglishAttributes.Instance, adTree);
            //Assert.AreEqual("book", word.Value);


            //// AdTree for the word containing a suffix.
            //adTree = new AdTree(new ConstructiveAdpositionalGrammar.Morphemes.Morpheme(myAttributesModel, "", EnglishAttributes.O.Lexeme),
            //    EnglishPattern.I_to_O_ing)
            //{
            //    Right = new AdTree(new ConstructiveAdpositionalGrammar.Morphemes.Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme), EnglishPattern.I_Lexeme_Verb),
            //    Left = new AdTree(new ConstructiveAdpositionalGrammar.Morphemes.Morpheme(myAttributesModel, "ing", EnglishAttributes.I.NonLexeme.Suffix), EnglishPattern.I_Suffix_ing),
            //};

            //word = new Word(EnglishAttributes.Instance, adTree);
            //Assert.AreEqual("reading", word.Value);
        }
    }
}
