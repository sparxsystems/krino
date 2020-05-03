using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
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
        private List<IPattern> myLexemePatterns;
        private MultiDictionaryUniqueValue<string, IMorpheme> myLexemes;

        public ConstructiveDictionary(IEnumerable<IMorpheme> lexemes, IEnumerable<IPattern> patterns)
        {
            Patterns = patterns;

            myLexemePatterns = patterns.Where(x => !x.MorphemeRule.Equals(MorphemeRule.Epsilon)).ToList();

            InitializeLexemes(lexemes);
            InitializePatternGraph();
        }

        public IEnumerable<IMorpheme> FindLexemes(string morph)
        {
            myLexemes.TryGetValues(morph, out ISet<IMorpheme> result);
            return result;
        }

        public IEnumerable<IPattern> FindLexemeMatchingPatterns(IMorpheme lexeme)
        {
            IEnumerable<IPattern> result = myLexemePatterns.Where(x => x.MorphemeRule.IsMatch(lexeme.Morph, lexeme.Attributes));
            return result;
        }

        public IEnumerable<IPattern> Patterns { get; private set; }

        public IDirectedGraph<GrammarCharacter, IPattern> PatternGraph { get; private set; }

        private void InitializeLexemes(IEnumerable<IMorpheme> lexemes)
        {
            MorphemeEqualityComparer morphemeEqualityComparer = new MorphemeEqualityComparer();
            myLexemes = new MultiDictionaryUniqueValue<string, IMorpheme>(EqualityComparer<string>.Default, morphemeEqualityComparer);
            foreach (IMorpheme lexeme in lexemes)
            {
                myLexemes.Add(lexeme.Morph, lexeme);
            }
        }

        private void InitializePatternGraph()
        {
            GrammarCharacter[] grammarCharacters = Enum.GetValues(typeof(GrammarCharacter)).Cast<GrammarCharacter>().ToArray();

            PatternGraph = new DirectedGraph<GrammarCharacter, IPattern>();
            foreach (GrammarCharacter grammarCharacter in grammarCharacters)
            {
                PatternGraph.AddVertex(grammarCharacter.ToString(), grammarCharacter);
            }

            foreach (IPattern pattern in Patterns)
            {
                // If it is an adposition related rule.
                if (!pattern.LeftRule.Equals(PatternRule.Nothing) && !pattern.RightRule.Equals(PatternRule.Nothing))
                {
                    foreach (GrammarCharacter leftGrammarCharacter in grammarCharacters)
                    {
                        if (pattern.LeftRule.IsMatch(leftGrammarCharacter))
                        {
                            foreach (GrammarCharacter rightGrammarCharacter in grammarCharacters)
                            {
                                if (pattern.RightRule.IsMatch(rightGrammarCharacter))
                                {
                                    PatternGraph.AddEdge(leftGrammarCharacter.ToString(), rightGrammarCharacter.ToString(), pattern);
                                    PatternGraph.AddEdge(rightGrammarCharacter.ToString(), leftGrammarCharacter.ToString(), pattern);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
