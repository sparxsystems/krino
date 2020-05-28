using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.GretaTest
{
    [TestFixture]
    public class GretaTest
    {
        // I have some good news and some bad news regarding the climate emergency.
        [Test]
        public void Sentence_1_1()
        {
            ConstructiveDictionary dictionary = new ConstructiveDictionary(MorphemeProvider.Morphemes, PatternProvider.Patterns);
            Parser parser = new Parser(dictionary);
            IAdTree adTree = parser.Deserialize("I have some good news and some bad news regarding the climate emergency.", 1);

            string s = parser.Serialize(adTree);

            List<string> phraseElements = adTree.GetPhraseElementsAsync().Result
                .Where(x => !string.IsNullOrEmpty(x.Morpheme.Morph))
                .Select(x => string.Join("->", string.Join("-", new IAdTree[] { x }.Concat(x.AdPositions).Select(y => y.IsOnLeft ? "L" : y.IsOnRight ? "R" : "").Reverse()), x.Morpheme.Morph))
                .ToList();

            Assert.AreEqual("i", adTree.Right.Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("have", adTree.Right.Right.Right.Right.Morpheme.Morph);
            Assert.AreEqual("some", adTree.Right.Right.Left.Right.Left.Morpheme.Morph);
            Assert.AreEqual("good", adTree.Right.Right.Left.Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("news", adTree.Right.Right.Left.Right.Right.Right.Morpheme.Morph);
            Assert.AreEqual("and", adTree.Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("some", adTree.Right.Right.Left.Left.Left.Morpheme.Morph);
            Assert.AreEqual("bad", adTree.Right.Right.Left.Left.Right.Left.Morpheme.Morph);
            Assert.AreEqual("news", adTree.Right.Right.Left.Left.Right.Right.Morpheme.Morph);
            Assert.AreEqual("regarding", adTree.Right.Morpheme.Morph);
            Assert.AreEqual("the", adTree.Right.Left.Left.Morpheme.Morph);
            Assert.AreEqual("climate", adTree.Right.Left.Right.Left.Right.Morpheme.Morph);
            Assert.AreEqual("emergency", adTree.Right.Left.Right.Right.Morpheme.Morph);
            Assert.AreEqual(".", adTree.Morpheme.Morph);
        }

        // I will start with the good news.
        [Test]
        public void Sentence_1_2()
        {
            ConstructiveDictionary dictionary = new ConstructiveDictionary(MorphemeProvider.Morphemes, PatternProvider.Patterns);
            Parser parser = new Parser(dictionary);
            IAdTree adTree = parser.Deserialize("I will start with the good news.", 1);

            string s = parser.Serialize(adTree);

            List<string> phraseElements = adTree.GetPhraseElementsAsync().Result
                .Where(x => !string.IsNullOrEmpty(x.Morpheme.Morph))
                .Select(x => string.Join("->", string.Join("-", new IAdTree[] { x }.Concat(x.AdPositions).Select(y => y.IsOnLeft ? "L" : y.IsOnRight ? "R" : "").Reverse()), x.Morpheme.Morph))
                .ToList();

            Assert.AreEqual("i", adTree.Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("will", adTree.Right.Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("start", adTree.Right.Right.Right.Right.Morpheme.Morph);
            Assert.AreEqual("with", adTree.Right.Morpheme.Morph);
            Assert.AreEqual("the", adTree.Right.Left.Left.Morpheme.Morph);
            Assert.AreEqual("good", adTree.Right.Left.Right.Left.Morpheme.Morph);
            Assert.AreEqual("news", adTree.Right.Left.Right.Right.Morpheme.Morph);
            Assert.AreEqual(".", adTree.Morpheme.Morph);
        }

        // The world, as a small number of people have been saying lately, will not end in 11 years.
        [Test]
        public void Sentence_2_1()
        {
            ConstructiveDictionary dictionary = new ConstructiveDictionary(MorphemeProvider.Morphemes, PatternProvider.Patterns);
            Parser parser = new Parser(dictionary);
            IAdTree adTree = parser.Deserialize("The world will not end in 11 years.", 1);

            string s = parser.Serialize(adTree);

            List<string> phraseElements = adTree.GetPhraseElementsAsync().Result
                .Where(x => !string.IsNullOrEmpty(x.Morpheme.Morph))
                .Select(x => string.Join("->", string.Join("-", new IAdTree[] { x }.Concat(x.AdPositions).Select(y => y.IsOnLeft ? "L" : y.IsOnRight ? "R" : "").Reverse()), x.Morpheme.Morph))
                .ToList();
        }
    }
}
