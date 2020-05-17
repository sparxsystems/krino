using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Vertical.Utils.Graphs;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.ConstructiveDictionaries
{
    [TestFixture]
    public class ConstructiveDictionaryTest
    {
        [Test]
        public void Constructor()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb },
                new Morpheme("s") { Attributes = Attributes.I.NonLexeme.VerbSuffix },
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern("I")
                {
                    MorphemeRule = MorphemeRule.I_Lexeme,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern("-s")
                {
                    MorphemeRule = new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Suffix("s"), MaskRule.Is(Attributes.I.NonLexeme.VerbSuffix)),
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                // Transference pattern.
                new Pattern("I>I")
                {
                    MorphemeRule = MorphemeRule.I,
                    RightRule = PatternRule.I_Lexeme,
                    LeftRule = new PatternRule(new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Anything, MaskRule.Is(Attributes.I.NonLexeme.VerbSuffix)), MaskRule.Anything),
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);
            
            // Note: the grammar characters are vertices.
            //       the path consists of edges.
            //       The edge is represented by the pattern which can connect two grammar characters.
            List<IReadOnlyList<DirectedEdge<Pattern>>> paths = dictionary.PatternGraph.FindAllPaths("I", "I").ToList();
            
            Assert.AreEqual(1, paths.Count);
            Assert.AreEqual(1, paths[0].Count);

            Assert.AreEqual(GrammarCharacter.I.ToString(), paths[0][0].From);
            Assert.AreEqual(GrammarCharacter.I.ToString(), paths[0][0].To);
        }

        [Test]
        public void FindLexemes_Similar()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("write") { Attributes = Attributes.I.Lexeme.Verb },
                new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun },
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
                new Morpheme("ex") { Attributes = Attributes.O.NonLexeme.NounPrefix },
                new Morpheme("extra") { Attributes = Attributes.O.NonLexeme.NounPrefix },
                new Morpheme("re") { Attributes = Attributes.O.NonLexeme.NounPrefix },
                new Morpheme("er") { Attributes = Attributes.O.NonLexeme.NounSuffix },
                new Morpheme("less") { Attributes = Attributes.A.NonLexeme.AdjectiveSuffix },
                new Morpheme("write") { Attributes = Attributes.I.Lexeme },
                new Morpheme("read") { Attributes = Attributes.I.Lexeme },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, null);

            // prefix 're'
            List<IReadOnlyList<Morpheme>> result = dictionary.FindMorphemeSequences("rewrite", 0).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(2, result[0].Count);
            Assert.AreEqual("re", result[0][0].Morph);
            Assert.AreEqual("write", result[0][1].Morph);

            // suffix 'er'
            result = dictionary.FindMorphemeSequences("reader", 0).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(2, result[0].Count);
            Assert.AreEqual("read", result[0][0].Morph);
            Assert.AreEqual("er", result[0][1].Morph);

            // multiple prefixes and sufixes
            result = dictionary.FindMorphemeSequences("extrarereaderless", 0).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(5, result[0].Count);
            Assert.AreEqual("extra", result[0][0].Morph);
            Assert.AreEqual("re", result[0][1].Morph);
            Assert.AreEqual("read", result[0][2].Morph);
            Assert.AreEqual("er", result[0][3].Morph);
            Assert.AreEqual("less", result[0][4].Morph);


            // 're' is the prefix but 'bla' is not a known lexeme.
            result = dictionary.FindMorphemeSequences("rebla", 0).ToList();
            Assert.AreEqual(0, result.Count);
        }

        [Test]
        public void FindMorphemeSequences_SuffixChangesLexeme()
        {
            List<Morpheme> morphemes = new List<Morpheme>()
            {
                new Morpheme("write") { Attributes = Attributes.I.Lexeme.Verb },
                new Morpheme("er") { Attributes = Attributes.O.NonLexeme.NounSuffix },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, null);

            List<IReadOnlyList<Morpheme>> morphemeSequences = dictionary.FindMorphemeSequences("writer", 1).ToList();
            Assert.AreEqual(2, morphemeSequences.Count);

            // 'writer' is very similar to write so it will just return 'write'.
            Assert.AreEqual(1, morphemeSequences[0].Count);
            Assert.AreEqual("write", morphemeSequences[0][0].Morph);

            // Then it shall also return the sequence with the recognized suffix.
            Assert.AreEqual(2, morphemeSequences[1].Count);
            Assert.AreEqual("write", morphemeSequences[1][0].Morph);
            Assert.AreEqual("er", morphemeSequences[1][1].Morph);
        }
    }
}
