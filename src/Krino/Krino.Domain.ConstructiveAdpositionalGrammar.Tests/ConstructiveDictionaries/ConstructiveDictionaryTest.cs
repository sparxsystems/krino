using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
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
            List<IReadOnlyList<DirectedEdge<IPattern>>> paths = dictionary.PatternGraph.FindAllPaths("I", "I").ToList();
            Assert.AreEqual(1, paths.Count);
            Assert.AreEqual(2, paths[0].Count);
            Assert.AreEqual(GrammarCharacter.I.ToString(), paths[0][0].From);
            Assert.AreEqual(GrammarCharacter.Epsilon.ToString(), paths[0][0].To);
            Assert.AreEqual(GrammarCharacter.Epsilon.ToString(), paths[0][1].From);
            Assert.AreEqual(GrammarCharacter.I.ToString(), paths[0][1].To);
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
            List<IReadOnlyList<IMorpheme>> result = dictionary.FindMorphemeSequences("rewrite").ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(2, result[0].Count);
            Assert.AreEqual("re", result[0][0].Morph);
            Assert.AreEqual("write", result[0][1].Morph);

            // suffix 'er'
            result = dictionary.FindMorphemeSequences("reader").ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(2, result[0].Count);
            Assert.AreEqual("read", result[0][0].Morph);
            Assert.AreEqual("er", result[0][1].Morph);

            // multiple prefixes and sufixes
            result = dictionary.FindMorphemeSequences("extrarereaderless").ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(5, result[0].Count);
            Assert.AreEqual("extra", result[0][0].Morph);
            Assert.AreEqual("re", result[0][1].Morph);
            Assert.AreEqual("read", result[0][2].Morph);
            Assert.AreEqual("er", result[0][3].Morph);
            Assert.AreEqual("less", result[0][4].Morph);


            // 're' is the prefix but 'bla' is not a known lexeme.
            result = dictionary.FindMorphemeSequences("rebla").ToList();
            Assert.AreEqual(0, result.Count);
        }
    }
}
