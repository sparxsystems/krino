using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishDictionary;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class ParserTest
    {
        [Test]
        public void Parse_SimpleSentence()
        {
            var dictionary = new ConstructiveDictionary(MorphemeProvider.AttributesModel, MorphemeProvider.Morphemes, PatternProvider.Patterns);
            var parser = new Parser(dictionary);

            var sentences = parser.Parse("I read book.");
            
            Assert.AreEqual(1, sentences.Count);
            Assert.AreEqual("i read book .", sentences[0].Value);
        }

        [Test]
        public void Parse_Argument()
        {
            var dictionary = new ConstructiveDictionary(MorphemeProvider.AttributesModel, MorphemeProvider.Morphemes, PatternProvider.Patterns);
            var parser = new Parser(dictionary);

            var sentences = parser.Parse("Book is book because book is book");

            Assert.AreEqual(1, sentences.Count);
            Assert.AreEqual("cycling on the grass is prohibited because walking on the grass is prohibited .", sentences[0].Value);
        }
    }
}
