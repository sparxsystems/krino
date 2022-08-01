using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Morphology;
using Krino.EnglishDictionary;
using Krino.EnglishGrammar.Morphology;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.ConstructiveGrammar.Tests.Morphology
{
    [TestFixture]
    public class MorphemeParserTest
    {
        [Test]
        public void Split()
        {
            var morphology = new EnglishMorphology();
            var parser = new MorphemeParser(morphology, MorphemeProvider.Morphemes);

            var result = parser.Split("hello, world-wide.");
            CollectionAssert.AreEqual(new string[] { "hello", ",", "world-wide", "." }, result);
        }

        [Test]
        public void ParseWord_Affixes()
        {
            var morphology = new EnglishMorphology();
            var parser = new MorphemeParser(morphology, MorphemeProvider.Morphemes);

            var words = parser.ParseWord("cycling").ToList();
            Assert.AreEqual(1, words.Count);
            Assert.IsTrue(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Gerund.IsIn(words[0].Attributes));
            Assert.AreEqual(1, words[0].Roots.Count);
            Assert.AreEqual("cycle", words[0].Roots[0].Value);
            Assert.AreEqual(1, words[0].Suffixes.Count);
            Assert.AreEqual("ing", words[0].Suffixes[0].Value);

            words = parser.ParseWord("recycling").ToList();
            Assert.AreEqual(1, words.Count);
            Assert.IsTrue(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Gerund.IsIn(words[0].Attributes));
            Assert.AreEqual(1, words[0].Roots.Count);
            Assert.AreEqual("cycle", words[0].Roots[0].Value);
            Assert.AreEqual(1, words[0].Suffixes.Count);
            Assert.AreEqual("ing", words[0].Suffixes[0].Value);
            Assert.AreEqual(1, words[0].Prefixes.Count);
            Assert.AreEqual("re", words[0].Prefixes[0].Value);

            words = parser.ParseWord("xxxing").ToList();
            Assert.AreEqual(0, words.Count);
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

            var dictionary = new MorphemeParser(morphology, morphemes);

            // prefix 're'
            var result = dictionary.ParseWord("rewrite").ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(1, result[0].Prefixes.Count);
            Assert.AreEqual("re", result[0].Prefixes[0].Value);
            Assert.AreEqual("write", result[0].Roots[0].Value);

            // suffix 'er'
            result = dictionary.ParseWord("reader").ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(1, result[0].Suffixes.Count);
            Assert.AreEqual("read", result[0].Roots[0].Value);
            Assert.AreEqual("er", result[0].Suffixes[0].Value);

            // multiple prefixes and sufixes
            result = dictionary.ParseWord("extrarereaderless").ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(2, result[0].Prefixes.Count);
            Assert.AreEqual(2, result[0].Suffixes.Count);
            Assert.AreEqual("extra", result[0].Prefixes[0].Value);
            Assert.AreEqual("re", result[0].Prefixes[1].Value);
            Assert.AreEqual("read", result[0].Roots[0].Value);
            Assert.AreEqual("er", result[0].Suffixes[0].Value);
            Assert.AreEqual("less", result[0].Suffixes[1].Value);


            // 're' is the prefix but 'bla' is not a known lexeme.
            result = dictionary.ParseWord("rebla").ToList();
            Assert.AreEqual(0, result.Count);
        }

        [Test]
        public void FindMorphemeSequences_DropE()
        {
            var morphology = new EnglishMorphology();

            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("write", GrammarAttributes.Morpheme.Free.Lexical.Verb),
                new Morpheme("er", GrammarAttributes.Morpheme.Bound.Suffix) { Binding = AffixBinding.Suffix("er") },
            };

            var dictionary = new MorphemeParser(morphology, morphemes);

            var morphemeSequences = dictionary.ParseWord("writer").ToList();
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
