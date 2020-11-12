using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Graphs;
using Krino.Vertical.Utils.Rules;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    /// <summary>
    /// Implements the constructive dictionary.
    /// </summary>
    public class ConstructiveDictionary : IConstructiveDictionary
    {
        private MultiKeyDistinctValueDictionary<string, Morpheme> myLexemes;
        private MultiKeyDistinctValueDictionary<string, Morpheme> myNonLexemes;

        public ConstructiveDictionary(IEnumerable<Morpheme> morphemes, IEnumerable<Pattern> patterns)
        {
            morphemes = morphemes ?? Enumerable.Empty<Morpheme>();
            Patterns = patterns ?? Enumerable.Empty<Pattern>();

            InitializeMorphemes(morphemes);
            InitializePatternGraph();
        }


        public IEnumerable<Morpheme> NonLexemes => myNonLexemes.Select(x => x.Value);

        public IEnumerable<Pattern> Patterns { get; private set; }

        public IDirectedGraph<GrammarCharacter, Pattern> PatternGraph { get; private set; }


        public IEnumerable<Morpheme> FindLexemes(string word, int maxDistance)
        {
            IEnumerable<Morpheme> result = Enumerable.Empty<Morpheme>();

            // Try to find exact lexemes.
            if (myLexemes.TryGetValues(word, out ReadOnlySet<Morpheme> lexemes))
            {
                result = lexemes;
            }

            if (maxDistance > 0)
            {
                // Also try to find lexemes which have similar morph.
                IEnumerable<string> similarMorphs = myLexemes.Keys.FindSimilar(word, maxDistance);

                if (lexemes != null)
                {
                    // Note: skip morphemes already returned among exact lexemes.
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

        public IReadOnlyList<IReadOnlyList<Morpheme>> DecomposeWord(string word, int maxMorphDistance)
        {
            List<IReadOnlyList<Morpheme>> wordConstructions = FindPossibleWordConstructions(word, maxMorphDistance, new List<Morpheme>())
                // Note: as an input parameter there is the list which is filled during the iteration.
                //       Therefore it must be iterated in once - so it must be converted to the list.
                .ToList();

            List<Morpheme> nonLexemes = FindNonLexemes(word).ToList();

            List<IReadOnlyList<Morpheme>> result = new List<IReadOnlyList<Morpheme>>();
            if (wordConstructions.Any())
            {
                result.AddRange(wordConstructions);
            }
            if (nonLexemes.Any())
            {
                result.Add(nonLexemes);
            }

            return result;
        }

        public IReadOnlyList<IReadOnlyList<IReadOnlyList<Morpheme>>> DecomposePhrase(IEnumerable<string> phrase, int maxMorphDistance)
        {
            List<IReadOnlyList<IReadOnlyList<Morpheme>>> result = new List<IReadOnlyList<IReadOnlyList<Morpheme>>>();

            // Go via each word.
            foreach (string word in phrase)
            {
                IReadOnlyList<IReadOnlyList<Morpheme>> possibleWordConstructions = DecomposeWord(word, maxMorphDistance);

                if (possibleWordConstructions.Any())
                {
                    result.Add(possibleWordConstructions);
                }
            }

            return result;
        }

        public IEnumerable<Pattern> FindPatterns(Morpheme morpheme)
        {
            IEnumerable<Pattern> result = Patterns
                .Where(x => x.MorphemeRule.GrammarCharacter != GrammarCharacter.e &&
                            !x.IsGrammarCharacterTransference() &&
                            x.MorphemeRule.Evaluate(morpheme));
            return result;
        }

        public IEnumerable<Pattern> FindGrammarCharacterTransferencePatterns(Morpheme morpheme)
        {
            IEnumerable<Pattern> result = Patterns
                .Where(x => x.IsGrammarCharacterTransference() && x.RightRule.Evaluate(morpheme));

            return result;
        }

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
            // Defines how patterns connect grammar characters.
            PatternGraph = new DirectedGraph<GrammarCharacter, Pattern>();
            PatternGraph.AddVertices(GrammarCharacterExt.GetValues());

            foreach (Pattern pattern in Patterns)
            {
                // If the pattern connects two grammar characters.
                bool isEdge = pattern.LeftRule.GrammarCharacter != GrammarCharacter.e &&
                    pattern.RightRule.GrammarCharacter != GrammarCharacter.e;

                if (isEdge)
                {
                    PatternGraph.AddEdge(pattern.LeftRule.GrammarCharacter, pattern.RightRule.GrammarCharacter, pattern);

                    if (pattern.LeftRule.GrammarCharacter != pattern.RightRule.GrammarCharacter)
                    {
                        PatternGraph.AddEdge(pattern.RightRule.GrammarCharacter, pattern.LeftRule.GrammarCharacter, pattern);
                    }
                }
            }
        }


        private IEnumerable<IReadOnlyList<Morpheme>> FindPossibleWordConstructions(
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
                        IEnumerable<IReadOnlyList<Morpheme>> sequences = FindPossibleWordConstructions(newWord, morphDistance, localSequence);
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
