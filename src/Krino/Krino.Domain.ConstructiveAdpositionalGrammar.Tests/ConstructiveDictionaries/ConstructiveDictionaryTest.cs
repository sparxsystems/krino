using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Vertical.Utils.Graphs;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Security.Cryptography.X509Certificates;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.ConstructiveDictionaries
{
    [TestFixture]
    public class ConstructiveDictionaryTest
    {
        [Test]
        public void PatternGraph_Basic()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern("A")
                {
                    MorphemeRule = MorphemeRule.A_Lexeme,
                    LeftRule = MorphemeRule.Nothing,
                    RightRule = MorphemeRule.Nothing,
                },

                new Pattern("O")
                {
                    MorphemeRule = MorphemeRule.O_Lexeme,
                    LeftRule = MorphemeRule.Nothing,
                    RightRule = MorphemeRule.Nothing,
                },

                new Pattern("A-O")
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    LeftRule = MorphemeRule.A_Lexeme,
                    RightRule = MorphemeRule.O_Lexeme,
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(new Morpheme[0], patterns);

            Assert.AreEqual(6, dictionary.PatternGraph.Count);
            Assert.AreEqual(2, dictionary.PatternGraph.Edges.Count());
            Assert.IsTrue(dictionary.PatternGraph.Edges.Any(x => x.From == GrammarCharacter.A && x.To == GrammarCharacter.O && x.Value.Name == "A-O"));
            Assert.IsTrue(dictionary.PatternGraph.Edges.Any(x => x.From == GrammarCharacter.O && x.To == GrammarCharacter.A && x.Value.Name == "A-O"));
        }

        [Test]
        public void PatternGraph_Itself()
        {
            List<Pattern> patterns = new List<Pattern>()
            {
                Pattern.EpsilonAdPosition("A-A", Attributes.A.Lexeme, Attributes.A.Lexeme),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(new Morpheme[0], patterns);

            Assert.AreEqual(1, dictionary.PatternGraph.Edges.Count());
            Assert.IsTrue(dictionary.PatternGraph.Edges.Any(x => x.From == GrammarCharacter.A && x.To == GrammarCharacter.A && x.Value.Name == "A-A"));
        }


        [Test]
        public void FindLexemes_Similar()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("write", Attributes.I.Lexeme.Verb),
                new Morpheme("book", Attributes.O.Lexeme.Noun),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, null);

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
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("ex", Attributes.O.NonLexeme.Prefix),
                new Morpheme("extra", Attributes.O.NonLexeme.Prefix),
                new Morpheme("re", Attributes.O.NonLexeme.Prefix),
                new Morpheme("er", Attributes.O.NonLexeme.Suffix),
                new Morpheme("less", Attributes.A.NonLexeme.Suffix),
                new Morpheme("write", Attributes.I.Lexeme),
                new Morpheme("read", Attributes.I.Lexeme),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, null);

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
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("write", Attributes.I.Lexeme.Verb),
                new Morpheme("er", Attributes.O.NonLexeme.Suffix),
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, null);

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
