using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules.Strings;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Vertical.Utils.Graphs;
using Krino.Vertical.Utils.Rules;
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
            List<IMorpheme> morphemes = new List<IMorpheme>()
            {
                new Morpheme("read") { Attributes = Attributes.I.Verb },
                new Morpheme("s") { Attributes = Attributes.I | Attributes.NonLexeme.Affix.Suffix },
            };

            List<IPattern> patterns = new List<IPattern>()
            {
                new Pattern("I")
                {
                    MorphemeRule = new MorphemeRule(Rule.Anything<string>(), Rule.Is(GrammarCharacter.I), MaskRule.Is(Attributes.I).And(MaskRule.Is(Attributes.NonLexeme).Not())),
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern("-s")
                {
                    MorphemeRule = new MorphemeRule(SuffixRule.Is("s"), Rule.Is(GrammarCharacter.I), MaskRule.Is(Attributes.I).And(MaskRule.Is(Attributes.NonLexeme.Affix.Suffix))),
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                // Transference pattern.
                new Pattern("I-s")
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(new MorphemeRule(Rule.Anything<string>(), Rule.Is(GrammarCharacter.I), MaskRule.Is(Attributes.I).And(MaskRule.Is(Attributes.NonLexeme.Affix.Suffix)))),
                },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, patterns);
            
            // Note: the grammar characters are vertices.
            //       the path consists of edges.
            //       The edge is represented by the pattern which can connect two grammar characters.
            List<IReadOnlyList<DirectedEdge<IPattern>>> paths = dictionary.PatternGraph.FindAllPaths("I", "I").ToList();
            
            Assert.AreEqual(1, paths.Count);
            Assert.AreEqual(1, paths[0].Count);

            Assert.AreEqual(GrammarCharacter.I.ToString(), paths[0][0].From);
            Assert.AreEqual(GrammarCharacter.I.ToString(), paths[0][0].To);
        }

        [Test]
        public void FindLexemes_Similar()
        {
            List<IMorpheme> morphemes = new List<IMorpheme>()
            {
                new Morpheme("write") { Attributes = Attributes.I.Verb },
                new Morpheme("book") { Attributes = Attributes.O.Noun },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, null);

            List<IMorpheme> result = dictionary.FindLexemes("writ", 1).ToList();
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
            List<IMorpheme> morphemes = new List<IMorpheme>()
            {
                new Morpheme("ex") { Attributes = Attributes.NonLexeme.Affix.Prefix },
                new Morpheme("extra") { Attributes = Attributes.NonLexeme.Affix.Prefix },
                new Morpheme("re") { Attributes = Attributes.NonLexeme.Affix.Prefix },
                new Morpheme("er") { Attributes = Attributes.NonLexeme.Affix.Suffix },
                new Morpheme("less") { Attributes = Attributes.NonLexeme.Affix.Suffix },
                new Morpheme("write") { Attributes = Attributes.I },
                new Morpheme("read") { Attributes = Attributes.I },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, null);

            // prefix 're'
            List<IReadOnlyList<IMorpheme>> result = dictionary.FindMorphemeSequences("rewrite", 0).ToList();
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
            List<IMorpheme> morphemes = new List<IMorpheme>()
            {
                new Morpheme("write") { Attributes = Attributes.I.Verb },
                new Morpheme("er") { Attributes = Attributes.O | Attributes.NonLexeme.Affix.Suffix },
            };

            ConstructiveDictionary dictionary = new ConstructiveDictionary(morphemes, null);

            List<IReadOnlyList<IMorpheme>> morphemeSequences = dictionary.FindMorphemeSequences("writer", 1).ToList();
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
