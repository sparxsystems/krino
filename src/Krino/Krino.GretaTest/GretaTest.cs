﻿using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.GretaTest
{
    [TestFixture]
    public class GretaTest
    {
        [Test]
        public void Sentence_1_1()
        {
            ConstructiveDictionary dictionary = new ConstructiveDictionary(MorphemeProvider.Morphemes, PatternProvider.Patterns);
            Parser parser = new Parser(dictionary);
            IAdTree adTree = parser.Deserialize("I have some good news and some bad news.", 1);

            string s = parser.Serialize(adTree);

            List<string> phraseElements = adTree.GetPhraseElementsAsync().Result
                .Where(x => !string.IsNullOrEmpty(x.Morpheme.Morph))
                .Select(x => string.Join("->", string.Join("-", new IAdTree[] { x }.Concat(x.AdPositions).Select(y => y.IsOnLeft ? "L" : y.IsOnRight ? "R" : "").Reverse()), x.Morpheme.Morph))
                .ToList();

            Assert.AreEqual("i", adTree.Right.Left.Morpheme.Morph);
            Assert.AreEqual("have", adTree.Right.Right.Morpheme.Morph);
            Assert.AreEqual("some", adTree.Left.Right.Left.Morpheme.Morph);
            Assert.AreEqual("good", adTree.Left.Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("news", adTree.Left.Right.Right.Right.Morpheme.Morph);
            Assert.AreEqual("and", adTree.Left.Morpheme.Morph);
            Assert.AreEqual("some", adTree.Left.Left.Left.Morpheme.Morph);
            Assert.AreEqual("bad", adTree.Left.Left.Right.Left.Morpheme.Morph);
            Assert.AreEqual("news", adTree.Left.Left.Right.Right.Morpheme.Morph);
        }
    }
}
