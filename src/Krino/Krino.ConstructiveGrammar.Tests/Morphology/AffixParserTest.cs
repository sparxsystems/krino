using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Morphology;
using Krino.EnglishGrammar.Morphology;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.ConstructiveGrammar.Tests.Morphology
{
    [TestFixture]
    public class AffixParserTest
    {
        [Test]
        public void FindFreeMorphemes_Similar()
        {
            var morphology = new EnglishMorphology();

            var morphemes = new List<Morpheme>()
            {
                new Morpheme("write", GrammarAttributes.Morpheme.Free.Lexical.Verb),
                new Morpheme("book", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            };

            var dictionary = new AffixParser(morphology, morphemes);

            var result = dictionary.ParseWord("writ", 1, 1).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("write", result[0].Value);

            // Try not similar but exactly matching morph.
            // Note: it cannot return two same morphemes (one exactly matching and then
            //       again the same morpheme as the similar one) but only one morpheme.
            result = dictionary.ParseWord("write", 1, 1).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("write", result[0].Value);
        }

        [Test]
        public void FindMorphemeSequences()
        {
            var morphology = new EnglishMorphology();

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("ex", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = AffixBinding.Prefix("ex") },
                new Morpheme("extra", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = AffixBinding.Prefix("extra") },
                new Morpheme("re", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = AffixBinding.Prefix("re") },
                new Morpheme("er", GrammarAttributes.Morpheme.Bound.Suffix) { Binding = AffixBinding.Suffix("er") },
                new Morpheme("less", GrammarAttributes.Morpheme.Bound.Suffix) { Binding = AffixBinding.Suffix("less") },
                new Morpheme("write", GrammarAttributes.Morpheme.Free.Lexical.Verb),
                new Morpheme("read", GrammarAttributes.Morpheme.Free.Lexical.Verb),
            };

            var dictionary = new AffixParser(morphology, morphemes);

            // prefix 're'
            var result = dictionary.ParseWord("rewrite", 0, 0).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(1, result[0].Prefixes.Count);
            Assert.AreEqual("re", result[0].Prefixes[0].Value);
            Assert.AreEqual("write", result[0].Roots[0].Value);

            // suffix 'er'
            result = dictionary.ParseWord("reader", 0, 0).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(1, result[0].Suffixes.Count);
            Assert.AreEqual("read", result[0].Roots[0].Value);
            Assert.AreEqual("er", result[0].Suffixes[0].Value);

            // multiple prefixes and sufixes
            result = dictionary.ParseWord("extrarereaderless", 0, 0).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(2, result[0].Prefixes.Count);
            Assert.AreEqual(2, result[0].Suffixes.Count);
            Assert.AreEqual("extra", result[0].Prefixes[0].Value);
            Assert.AreEqual("re", result[0].Prefixes[1].Value);
            Assert.AreEqual("read", result[0].Roots[0].Value);
            Assert.AreEqual("er", result[0].Suffixes[0].Value);
            Assert.AreEqual("less", result[0].Suffixes[1].Value);


            // 're' is the prefix but 'bla' is not a known lexeme.
            result = dictionary.ParseWord("rebla", 0, 0).ToList();
            Assert.AreEqual(0, result.Count);
        }

        [Test]
        public void FindMorphemeSequences_SuffixChangesFreeMorpheme()
        {
            var morphology = new EnglishMorphology();

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("write", GrammarAttributes.Morpheme.Free.Lexical.Verb),
                new Morpheme("er", GrammarAttributes.Morpheme.Bound.Suffix) { Binding = AffixBinding.Suffix("er") },
            };

            var dictionary = new AffixParser(morphology, morphemes);

            var morphemeSequences = dictionary.ParseWord("writer", 1, 1).ToList();
            Assert.AreEqual(2, morphemeSequences.Count);

            // 'writer' is very similar to write so it will just return 'write'.
            Assert.AreEqual(1, morphemeSequences[0].Morphemes.Count());
            Assert.AreEqual("write", morphemeSequences[0].Roots[0].Value);

            // Then it shall also return the sequence with the recognized suffix.
            Assert.AreEqual(2, morphemeSequences[1].Morphemes.Count());
            Assert.AreEqual("write", morphemeSequences[1].Roots[0].Value);
            Assert.AreEqual("er", morphemeSequences[1].Suffixes[0].Value);
        }
    }
}
