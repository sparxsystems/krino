﻿using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.ConstructiveDictionaries
{
    [TestFixture]
    public class ConstructiveDictionary2Test
    {
        [Test]
        public void FindFreeMorphemes_Similar()
        {
            var morphemes = new List<Morpheme>()
            {
                new Morpheme("write", GrammarAttributes.Morpheme.Free.Lexical.Verb),
                new Morpheme("book", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            };

            var dictionary = new ConstructiveDictionary2(morphemes);

            var result = dictionary.FindFreeMorphemes("writ", 1).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("write", result[0].Value);

            // Try not similar but exactly matching morph.
            // Note: it cannot return two same morphemes (one exactly matching and then
            //       again the same morpheme as the similar one) but only one morpheme.
            result = dictionary.FindFreeMorphemes("write", 1).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("write", result[0].Value);
        }

        [Test]
        public void FindMorphemeSequences()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("ex", GrammarAttributes.Morpheme.Bound.Prefix),
                new Morpheme("extra", GrammarAttributes.Morpheme.Bound.Prefix),
                new Morpheme("re", GrammarAttributes.Morpheme.Bound.Prefix),
                new Morpheme("er", GrammarAttributes.Morpheme.Bound.Suffix),
                new Morpheme("less", GrammarAttributes.Morpheme.Bound.Suffix),
                new Morpheme("write", GrammarAttributes.Morpheme.Free.Lexical.Verb),
                new Morpheme("read", GrammarAttributes.Morpheme.Free.Lexical.Verb),
            };

            var dictionary = new ConstructiveDictionary2(morphemes);

            // prefix 're'
            var result = dictionary.FindWords("rewrite", 0).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(1, result[0].Prefixes.Count);
            Assert.AreEqual("re", result[0].Prefixes[0].Value);
            Assert.AreEqual("write", result[0].Root.Value);

            // suffix 'er'
            result = dictionary.FindWords("reader", 0).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(1, result[0].Suffixes.Count);
            Assert.AreEqual("read", result[0].Root.Value);
            Assert.AreEqual("er", result[0].Suffixes[0].Value);

            // multiple prefixes and sufixes
            result = dictionary.FindWords("extrarereaderless", 0).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(2, result[0].Prefixes.Count);
            Assert.AreEqual(2, result[0].Suffixes.Count);
            Assert.AreEqual("extra", result[0].Prefixes[0].Value);
            Assert.AreEqual("re", result[0].Prefixes[1].Value);
            Assert.AreEqual("read", result[0].Root.Value);
            Assert.AreEqual("er", result[0].Suffixes[0].Value);
            Assert.AreEqual("less", result[0].Suffixes[1].Value);


            // 're' is the prefix but 'bla' is not a known lexeme.
            result = dictionary.FindWords("rebla", 0).ToList();
            Assert.AreEqual(0, result.Count);
        }

        [Test]
        public void FindMorphemeSequences_SuffixChangesFreeMorpheme()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("write", GrammarAttributes.Morpheme.Free.Lexical.Verb),
                new Morpheme("er", GrammarAttributes.Morpheme.Bound.Suffix),
            };

            var dictionary = new ConstructiveDictionary2(morphemes);

            var morphemeSequences = dictionary.FindWords("writer", 1).ToList();
            Assert.AreEqual(2, morphemeSequences.Count);

            // 'writer' is very similar to write so it will just return 'write'.
            Assert.AreEqual(1, morphemeSequences[0].Morphemes.Count());
            Assert.AreEqual("write", morphemeSequences[0].Root.Value);

            // Then it shall also return the sequence with the recognized suffix.
            Assert.AreEqual(2, morphemeSequences[1].Morphemes.Count());
            Assert.AreEqual("write", morphemeSequences[1].Root.Value);
            Assert.AreEqual("er", morphemeSequences[1].Suffixes[0].Value);
        }
    }
}
