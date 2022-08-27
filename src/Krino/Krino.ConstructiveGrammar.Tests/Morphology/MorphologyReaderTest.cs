using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Morphology;
using Krino.EnglishDictionary;
using Krino.EnglishGrammar.Morphology;
using NUnit.Framework;
using System.Linq;

namespace Krino.ConstructiveGrammar.Tests.Morphology
{
    [TestFixture]
    public class MorphologyReaderTest
    {
        [Test]
        public void SplitSentences()
        {
            var morphology = new EnglishMorphology();
            var reader = new MorphologyReader(morphology, MorphemeProvider.Morphemes);

            var result = reader.SplitSentences("hello, world-wide.");
            CollectionAssert.AreEqual(new string[] { "hello", ",", "world-wide", "." }, result);
        }

        [Test]
        public void IsPunctuationMark()
        {
            var morphology = new EnglishMorphology();
            var reader = new MorphologyReader(morphology, MorphemeProvider.Morphemes);

            Assert.IsTrue(reader.IsPunctuationMark("."));
            Assert.IsTrue(reader.IsPunctuationMark("-"));
            Assert.IsFalse(reader.IsPunctuationMark("a"));
        }

        [Test]
        public void IsEndOfSentencePunctuationMark()
        {
            var morphology = new EnglishMorphology();
            var reader = new MorphologyReader(morphology, MorphemeProvider.Morphemes);

            Assert.IsTrue(reader.IsEndOfSentencePunctuationMark("."));
            Assert.IsFalse(reader.IsEndOfSentencePunctuationMark("-"));
            Assert.IsFalse(reader.IsEndOfSentencePunctuationMark("a"));
        }

        [Test]
        public void ReadWord_Affixes()
        {
            var morphology = new EnglishMorphology();
            var reader = new MorphologyReader(morphology, MorphemeProvider.Morphemes);

            var words = reader.ReadWord("cycling").ToList();
            Assert.AreEqual(2, words.Count);
            
            Assert.IsTrue(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentParticiple.IsIn(words[0].Attributes));
            Assert.AreEqual(1, words[0].Roots.Count);
            Assert.AreEqual("cycle", words[0].Roots[0].Value);
            Assert.AreEqual(1, words[0].Suffixes.Count);
            Assert.AreEqual("ing", words[0].Suffixes[0].Value);

            Assert.IsTrue(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Gerund.IsIn(words[1].Attributes));
            Assert.AreEqual(1, words[1].Roots.Count);
            Assert.AreEqual("cycle", words[1].Roots[0].Value);
            Assert.AreEqual(1, words[1].Suffixes.Count);
            Assert.AreEqual("ing", words[1].Suffixes[0].Value);

            words = reader.ReadWord("recycling").ToList();
            Assert.AreEqual(2, words.Count);
            Assert.IsTrue(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentParticiple.IsIn(words[0].Attributes));
            Assert.AreEqual(1, words[0].Roots.Count);
            Assert.AreEqual("cycle", words[0].Roots[0].Value);
            Assert.AreEqual(1, words[0].Suffixes.Count);
            Assert.AreEqual("ing", words[0].Suffixes[0].Value);
            Assert.AreEqual(1, words[0].Prefixes.Count);
            Assert.AreEqual("re", words[0].Prefixes[0].Value);

            Assert.IsTrue(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Gerund.IsIn(words[1].Attributes));
            Assert.AreEqual(1, words[1].Roots.Count);
            Assert.AreEqual("cycle", words[1].Roots[0].Value);
            Assert.AreEqual(1, words[1].Suffixes.Count);
            Assert.AreEqual("ing", words[1].Suffixes[0].Value);
            Assert.AreEqual(1, words[1].Prefixes.Count);
            Assert.AreEqual("re", words[1].Prefixes[0].Value);

            // prefix 're'
            words = reader.ReadWord("rewrite").ToList();
            Assert.AreEqual(1, words.Count);
            Assert.AreEqual(1, words[0].Prefixes.Count);
            Assert.AreEqual("re", words[0].Prefixes[0].Value);
            Assert.AreEqual("write", words[0].Roots[0].Value);

            // suffix 'er'
            words = reader.ReadWord("reader").ToList();
            Assert.AreEqual(1, words.Count);
            Assert.AreEqual(1, words[0].Suffixes.Count);
            Assert.AreEqual("read", words[0].Roots[0].Value);
            Assert.AreEqual("er", words[0].Suffixes[0].Value);

            // multiple prefixes and sufixes
            words = reader.ReadWord("extrarereaderless").ToList();
            Assert.AreEqual(1, words.Count);
            Assert.AreEqual(2, words[0].Prefixes.Count);
            Assert.AreEqual(2, words[0].Suffixes.Count);
            Assert.AreEqual("extra", words[0].Prefixes[0].Value);
            Assert.AreEqual("re", words[0].Prefixes[1].Value);
            Assert.AreEqual("read", words[0].Roots[0].Value);
            Assert.AreEqual("er", words[0].Suffixes[0].Value);
            Assert.AreEqual("less", words[0].Suffixes[1].Value);


            // 're' is the prefix but 'bla' is not a known lexeme.
            words = reader.ReadWord("rebla").ToList();
            Assert.AreEqual(0, words.Count);

            words = reader.ReadWord("xxxing").ToList();
            Assert.AreEqual(0, words.Count);
        }

    }
}
