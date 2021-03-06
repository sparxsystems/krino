﻿using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.ConstructiveDictionaries
{
    [TestFixture]
    public class ConstructiveDictionaryTest
    {
        private EnglishAttributesModel myAttributesModel = new EnglishAttributesModel();


        [Test]
        public void FindLexemes_Similar()
        {
            IAttributesModel attributesModel = new EnglishAttributesModel();

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme(attributesModel, "write", EnglishAttributes.I.Lexeme.Verb),
                new Morpheme(attributesModel, "book", EnglishAttributes.O.Lexeme.Noun),
            };

            var patternConstructions = new PatternConstructions(5, new Pattern[0]);
            ConstructiveDictionary dictionary = new ConstructiveDictionary(attributesModel, patternConstructions, morphemes);

            List<Morpheme> result = dictionary.FindLexemes("writ", 1).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("write", result[0].Morph);

            // Try not similar but exactly matching morph.
            // Note: it cannot return two same morphemes (one exactly matching and then
            //       again the same morpheme as the similar one) but only one morpheme.
            result = dictionary.FindLexemes("write", 1).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("write", result[0].Morph);
        }

        [Test]
        public void FindMorphemeSequences()
        {
            IAttributesModel attributesModel = new EnglishAttributesModel();

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme(attributesModel, "ex", EnglishAttributes.O.NonLexeme.Prefix),
                new Morpheme(attributesModel, "extra", EnglishAttributes.O.NonLexeme.Prefix),
                new Morpheme(attributesModel, "re", EnglishAttributes.O.NonLexeme.Prefix),
                new Morpheme(attributesModel, "er", EnglishAttributes.O.NonLexeme.Suffix),
                new Morpheme(attributesModel, "less", EnglishAttributes.A.NonLexeme.Suffix),
                new Morpheme(attributesModel, "write", EnglishAttributes.I.Lexeme),
                new Morpheme(attributesModel, "read", EnglishAttributes.I.Lexeme),
            };

            var patternConstructions = new PatternConstructions(5, new Pattern[0]);
            ConstructiveDictionary dictionary = new ConstructiveDictionary(attributesModel, patternConstructions, morphemes);

            // prefix 're'
            var result = dictionary.DecomposeWord("rewrite", 0);
            Assert.AreEqual(1, result.Compositions.Count);
            Assert.AreEqual(2, result.Compositions[0].Morphemes.Count);
            Assert.AreEqual("re", result.Compositions[0].Morphemes[0].Morph);
            Assert.AreEqual("write", result.Compositions[0].Morphemes[1].Morph);

            // suffix 'er'
            result = dictionary.DecomposeWord("reader", 0);
            Assert.AreEqual(1, result.Compositions.Count);
            Assert.AreEqual(2, result.Compositions[0].Morphemes.Count);
            Assert.AreEqual("read", result.Compositions[0].Morphemes[0].Morph);
            Assert.AreEqual("er", result.Compositions[0].Morphemes[1].Morph);

            // multiple prefixes and sufixes
            result = dictionary.DecomposeWord("extrarereaderless", 0);
            Assert.AreEqual(1, result.Compositions.Count);
            Assert.AreEqual(5, result.Compositions[0].Morphemes.Count);
            Assert.AreEqual("extra", result.Compositions[0].Morphemes[0].Morph);
            Assert.AreEqual("re", result.Compositions[0].Morphemes[1].Morph);
            Assert.AreEqual("read", result.Compositions[0].Morphemes[2].Morph);
            Assert.AreEqual("er", result.Compositions[0].Morphemes[3].Morph);
            Assert.AreEqual("less", result.Compositions[0].Morphemes[4].Morph);


            // 're' is the prefix but 'bla' is not a known lexeme.
            result = dictionary.DecomposeWord("rebla", 0);
            Assert.AreEqual(0, result.Compositions.Count);
        }

        [Test]
        public void FindMorphemeSequences_SuffixChangesLexeme()
        {
            IAttributesModel attributesModel = new EnglishAttributesModel();

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme(attributesModel, "write", EnglishAttributes.I.Lexeme.Verb),
                new Morpheme(attributesModel, "er", EnglishAttributes.O.NonLexeme.Suffix),
            };

            var patternConstructions = new PatternConstructions(5, new Pattern[0]);
            ConstructiveDictionary dictionary = new ConstructiveDictionary(attributesModel, patternConstructions, morphemes);

            var morphemeSequences = dictionary.DecomposeWord("writer", 1);
            Assert.AreEqual(2, morphemeSequences.Compositions.Count);

            // 'writer' is very similar to write so it will just return 'write'.
            Assert.AreEqual(1, morphemeSequences.Compositions[0].Morphemes.Count);
            Assert.AreEqual("write", morphemeSequences.Compositions[0].Morphemes[0].Morph);

            // Then it shall also return the sequence with the recognized suffix.
            Assert.AreEqual(2, morphemeSequences.Compositions[1].Morphemes.Count);
            Assert.AreEqual("write", morphemeSequences.Compositions[1].Morphemes[0].Morph);
            Assert.AreEqual("er", morphemeSequences.Compositions[1].Morphemes[1].Morph);
        }

    }
}
