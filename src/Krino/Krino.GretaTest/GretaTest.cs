using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishDictionary;
using Krino.Domain.EnglishGrammar.Parsing;
using Krino.Vertical.Utils.Diagnostic;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.GretaTest
{
    [TestFixture]
    public class GretaTest
    {

        [Test]
        public void Sentence_1_1()
        {

            //Trace.StartProfiler();

            var dictionary = new EnglishConstructiveDictionaryFactory() { MaxWords = 8 }.Create();
            var grammar = new EnglishMachine();
            var parser = new Parser(dictionary, grammar.Machine);
            //var results = parser.Parse("I have some good news and some bad news regarding the climate emergency.");
            var results = parser.Parse("I have some good news and some bad news regarding the climate emergency.");

            Assert.AreEqual(1, results.Count);
        }

        // I will start with the good news.
        [Test]
        public void Sentence_1_2()
        {
            var dictionary = new EnglishConstructiveDictionaryFactory().Create();
            var grammar = new EnglishMachine();
            var parser = new Parser(dictionary, grammar.Machine);

            var results = parser.Parse("I will start with the good news.");

            Assert.AreEqual(1, results.Count);
        }

        // The world, as a small number of people have been saying lately, will not end in 11 years.
        [Test]
        public void Sentence_2_1()
        {
            //var dictionary = new EnglishConstructiveDictionaryFactory().Create();
            //AdTreeCreator adTreeCreator = new AdTreeCreator(dictionary);
            //List<IAdTree> results = adTreeCreator.Create("The world as a small number of people have been saying lately will not end in 11 years".ToLower().Split(" "));

            //List<string> phraseElements = adTree
            //    .Where(x => !string.IsNullOrEmpty(x.Morpheme.Morph))
            //    .Select(x => string.Join("->", string.Join("-", new IAdTree[] { x }.Concat(x.AdPositions).Select(y => y.IsOnLeft ? "L" : y.IsOnRight ? "R" : "").Reverse()), x.Morpheme.Morph))
            //    .ToList();
        }
    }
}
