using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Graphs;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    /// <summary>
    /// Implements the constructive dictionary.
    /// </summary>
    public class ConstructiveDictionary : IConstructiveDictionary
    {
        private List<Pattern> myLexemePatterns;
        private MultiKeyDistinctValueDictionary<string, Morpheme> myLexemes;
        private MultiKeyDistinctValueDictionary<string, Morpheme> myNonLexemes;

        public ConstructiveDictionary(IEnumerable<Morpheme> morphemes, IEnumerable<Pattern> patterns)
        {
            Patterns = patterns ?? Enumerable.Empty<Pattern>();

            myLexemePatterns = Patterns.Where(x => x.MorphemeRule.GrammarCharacter != GrammarCharacter.Epsilon).ToList();

            InitializeMorphemes(morphemes);
            InitializePatternGraph();
        }

        public IEnumerable<Morpheme> FindLexemes(string morph, int maxDistance)
        {
            IEnumerable<Morpheme> result = Enumerable.Empty<Morpheme>();

            // Try to find exact lexemes.
            if (myLexemes.TryGetValues(morph, out ReadOnlySet<Morpheme> lexemes))
            {
                result = lexemes;
            }

            if (maxDistance > 0)
            {
                // Also try to find lexemes which have similar morph.
                IEnumerable<string> similarMorphs = myLexemes.Keys.FindSimilar(morph, maxDistance);

                if (lexemes != null)
                {
                    // Note: exclude morphemes already returned among exact lexemes.
                    result = result.Concat(similarMorphs.SelectMany(x => myLexemes[x].Where(y => !lexemes.Contains(y))));
                }
                else
                {
                    result = result.Concat(similarMorphs.SelectMany(x => myLexemes[x]));
                }
            }

            return result;
        }

        public IEnumerable<Morpheme> FindNonLexemes(string morph)
        {
            myNonLexemes.TryGetValues(morph, out ReadOnlySet<Morpheme> result);
            return result ?? Enumerable.Empty<Morpheme>();
        }

        public IEnumerable<IReadOnlyList<Morpheme>> FindMorphemeSequences(string word, int maxMorphDistance)
        {
            IEnumerable<IReadOnlyList<Morpheme>> result = FindAllMorphemeSequences(word, maxMorphDistance, new List<Morpheme>());
            return result;
        }

        public IEnumerable<Pattern> FindMatchingPatterns(Morpheme lexeme)
        {
            IEnumerable<Pattern> result = myLexemePatterns.Where(x => x.MorphemeRule.IsMatch(lexeme.Morph, lexeme.Attributes));
            return result;
        }

        public IEnumerable<Morpheme> NonLexemes { get; }

        public IEnumerable<Pattern> Patterns { get; private set; }

        public IDirectedGraph<GrammarCharacter, Pattern> PatternGraph { get; private set; }

        private void InitializeMorphemes(IEnumerable<Morpheme> morphemes)
        {
            MorphemeEqualityComparer morphemeEqualityComparer = new MorphemeEqualityComparer();
            myLexemes = new MultiKeyDistinctValueDictionary<string, Morpheme>(EqualityComparer<string>.Default, morphemeEqualityComparer);
            myNonLexemes = new MultiKeyDistinctValueDictionary<string, Morpheme>(EqualityComparer<string>.Default, morphemeEqualityComparer);
            foreach (Morpheme morpheme in morphemes)
            {
                if (morpheme.IsLexeme)
                {
                    myLexemes.Add(morpheme.Morph, morpheme);
                }
                else
                {
                    myNonLexemes.Add(morpheme.Morph, morpheme);
                }
            }
        }

        private void InitializePatternGraph()
        {
            GrammarCharacter[] grammarCharacters = Enum.GetValues(typeof(GrammarCharacter)).Cast<GrammarCharacter>().ToArray();

            PatternGraph = new DirectedGraph<GrammarCharacter, Pattern>();
            foreach (GrammarCharacter grammarCharacter in grammarCharacters)
            {
                PatternGraph.AddVertex(grammarCharacter.ToString(), grammarCharacter);
            }

            foreach (Pattern pattern in Patterns)
            {
                // If it is an adposition related rule.
                if (!pattern.LeftRule.Equals(PatternRule.Nothing) && !pattern.RightRule.Equals(PatternRule.Nothing))
                {
                    foreach (GrammarCharacter leftGrammarCharacter in grammarCharacters)
                    {
                        if (pattern.LeftRule.MorphemeRule.GrammarCharacter == leftGrammarCharacter)
                        {
                            foreach (GrammarCharacter rightGrammarCharacter in grammarCharacters)
                            {
                                if (pattern.RightRule.MorphemeRule.GrammarCharacter == rightGrammarCharacter)
                                {
                                    PatternGraph.AddEdge(leftGrammarCharacter.ToString(), rightGrammarCharacter.ToString(), pattern);

                                    if (leftGrammarCharacter != rightGrammarCharacter)
                                    {
                                        PatternGraph.AddEdge(rightGrammarCharacter.ToString(), leftGrammarCharacter.ToString(), pattern);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }


        private IEnumerable<IReadOnlyList<Morpheme>> FindAllMorphemeSequences(
            string word,
            int morphDistance,
            List<Morpheme> localSequence)
        {
            // Find if the word is a lexeme.
            IEnumerable<Morpheme> lexemes = FindLexemes(word, morphDistance);
            foreach (Morpheme lexeme in lexemes)
            {
                localSequence.Add(lexeme);
                yield return localSequence.ToList();
                localSequence.RemoveAt(localSequence.Count - 1);
            }

            // Find if the word is a lexeme with suffixes.
            IEnumerable<IReadOnlyList<Morpheme>> wordSuffixes = FindLexemeAndItsSuffixes(word, morphDistance, new List<Morpheme>());
            foreach (IReadOnlyList<Morpheme> sequence in wordSuffixes)
            {
                yield return localSequence.Concat(sequence.Reverse()).ToList();
            }

            // Find if the word is a lexeme with prefixes and suffixes.
            for (int i = 1; i < word.Length; ++i)
            {
                string nonLexeme = word.Substring(0, i);
                IEnumerable<Morpheme> prefixHomonyms = FindNonLexemes(nonLexeme)
                    .Where(x => Attributes.IsPrefix(x.Attributes));
                if (prefixHomonyms.Any())
                {
                    string newWord = word.Substring(i);

                    foreach (Morpheme prefix in prefixHomonyms)
                    {
                        localSequence.Add(prefix);

                        // Try if there are sub-prefixes.
                        IEnumerable<IReadOnlyList<Morpheme>> sequences = FindAllMorphemeSequences(newWord, morphDistance, localSequence);
                        foreach (IReadOnlyList<Morpheme> sequence in sequences)
                        {
                            yield return sequence.ToList();
                        }

                        localSequence.RemoveAt(localSequence.Count - 1);
                    }
                }
            }

        }

        private IEnumerable<IReadOnlyList<Morpheme>> FindLexemeAndItsSuffixes(
            string word,
            int morphDistance,
            List<Morpheme> localSequence)
        {
            // If there is some suffix.
            if (localSequence.Count > 0)
            {
                // If the word is a lexeme.
                IEnumerable<Morpheme> lexemes = FindLexemes(word, morphDistance);
                foreach (Morpheme lexeme in lexemes)
                {
                    localSequence.Add(lexeme);
                    yield return localSequence.ToList();
                    localSequence.RemoveAt(localSequence.Count - 1);
                }
            }

            // Try to find suffixes in the word.
            for (int i = word.Length - 1; i > 0; --i)
            {
                string nonLexeme = word.Substring(i);
                IEnumerable<Morpheme> suffixes = FindNonLexemes(nonLexeme)
                    .Where(x => Attributes.IsSuffix(x.Attributes));
                if (suffixes.Any())
                {
                    string newWord = word.Substring(0, i);

                    foreach (Morpheme sufix in suffixes)
                    {
                        localSequence.Add(sufix);

                        IEnumerable<IReadOnlyList<Morpheme>> sequences = FindLexemeAndItsSuffixes(newWord, morphDistance, localSequence);
                        foreach (IReadOnlyList<Morpheme> sequence in sequences)
                        {
                            yield return sequence;
                        }

                        localSequence.RemoveAt(localSequence.Count - 1);
                    }
                }
            }
        }
    }
}
