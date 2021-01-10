using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.Morphemes;
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
        private EnglishAttributesModel myEnglishAttributesModel = new EnglishAttributesModel();

        [Test]
        public void Sentence_1_1()
        {

            //Trace.StartProfiler();

            ConstructiveDictionary dictionary = new ConstructiveDictionary(myEnglishAttributesModel, MorphemeProvider.Morphemes, PatternProvider.Patterns);
            AdTreeCreator adTreeCreator = new AdTreeCreator(myEnglishAttributesModel, dictionary);
            List<IAdTree> results = adTreeCreator.Create("I have some good news and some bad news regarding the climate emergency .".ToLower().Split(" "));

            //Trace.StopProfiler();

            Assert.AreEqual(2, results.Count);
            Assert.AreEqual("i", results[0].Right.Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("have", results[0].Right.Right.Right.Right.Morpheme.Morph);
            Assert.AreEqual("some", results[0].Right.Right.Left.Right.Left.Morpheme.Morph);
            Assert.AreEqual("good", results[0].Right.Right.Left.Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("news", results[0].Right.Right.Left.Right.Right.Right.Morpheme.Morph);
            Assert.AreEqual("and", results[0].Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("some", results[0].Right.Right.Left.Left.Left.Morpheme.Morph);
            Assert.AreEqual("bad", results[0].Right.Right.Left.Left.Right.Left.Morpheme.Morph);
            Assert.AreEqual("news", results[0].Right.Right.Left.Left.Right.Right.Morpheme.Morph);
            Assert.AreEqual("regarding", results[0].Right.Morpheme.Morph);
            Assert.AreEqual("the", results[0].Right.Left.Left.Morpheme.Morph);
            Assert.AreEqual("climate", results[0].Right.Left.Right.Left.Right.Morpheme.Morph);
            Assert.AreEqual("emergency", results[0].Right.Left.Right.Right.Morpheme.Morph);
            Assert.AreEqual(".", results[0].Morpheme.Morph);
        }

        // I will start with the good news.
        [Test]
        public void Sentence_1_2()
        {
            ConstructiveDictionary dictionary = new ConstructiveDictionary(myEnglishAttributesModel, MorphemeProvider.Morphemes, PatternProvider.Patterns);
            AdTreeCreator adTreeCreator = new AdTreeCreator(myEnglishAttributesModel, dictionary);
            List<IAdTree> results = adTreeCreator.Create("I will start with the good news .".ToLower().Split(" "));

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("i", results[0].Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("will", results[0].Right.Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("start", results[0].Right.Right.Right.Right.Morpheme.Morph);
            Assert.AreEqual("with", results[0].Right.Morpheme.Morph);
            Assert.AreEqual("the", results[0].Right.Left.Left.Morpheme.Morph);
            Assert.AreEqual("good", results[0].Right.Left.Right.Left.Morpheme.Morph);
            Assert.AreEqual("news", results[0].Right.Left.Right.Right.Morpheme.Morph);
            Assert.AreEqual(".", results[0].Morpheme.Morph);
        }

        // The world, as a small number of people have been saying lately, will not end in 11 years.
        [Test]
        public void Sentence_2_1()
        {
            ConstructiveDictionary dictionary = new ConstructiveDictionary(myEnglishAttributesModel, MorphemeProvider.Morphemes, PatternProvider.Patterns);
            AdTreeCreator adTreeCreator = new AdTreeCreator(myEnglishAttributesModel, dictionary);
            List<IAdTree> results = adTreeCreator.Create("The world as a small number of people have been saying lately will not end in 11 years".ToLower().Split(" "));

            //List<string> phraseElements = adTree
            //    .Where(x => !string.IsNullOrEmpty(x.Morpheme.Morph))
            //    .Select(x => string.Join("->", string.Join("-", new IAdTree[] { x }.Concat(x.AdPositions).Select(y => y.IsOnLeft ? "L" : y.IsOnRight ? "R" : "").Reverse()), x.Morpheme.Morph))
            //    .ToList();
        }
    }
}
