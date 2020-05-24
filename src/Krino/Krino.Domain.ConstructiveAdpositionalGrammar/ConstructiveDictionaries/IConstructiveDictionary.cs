using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Graphs;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    /// <summary>
    /// Declares the constructive dictionary as defined in the Constructive Adpositional Grammar book.
    /// </summary>
    public interface IConstructiveDictionary
    {
        /// <summary>
        /// Finds all lexemes which have similar or exactly matching morph.
        /// </summary>
        /// <param name="morph"></param>
        /// <param name="maxDistance">The level of similarity. If 0 then the exact morph is searched.</param>
        /// <returns></returns>
        IEnumerable<Morpheme> FindLexemes(string morph, int maxDistance);

        /// <summary>
        /// Finds all non-lexemes matching the morph.
        /// </summary>
        /// <param name="morph"></param>
        /// <returns></returns>
        IEnumerable<Morpheme> FindNonLexemes(string morph);

        /// <summary>
        /// Finds all possible combinations of morphemes for the given word.
        /// </summary>
        /// <param name="word"></param>
        /// <param name="maxMorphDistance">The level of similarity. If 0 then the exact morph is searched.</param>
        /// <returns></returns>
        IEnumerable<IReadOnlyList<Morpheme>> FindMorphemeSequences(string word, int maxMorphDistance);

        /// <summary>
        /// Finds patterns matching the lexeme.
        /// </summary>
        /// <param name="lexeme"></param>
        /// <returns></returns>
        IEnumerable<Pattern> FindMatchingPatterns(Morpheme lexeme);

        /// <summary>
        /// Finds primitive transference patterns for the morpheme.
        /// </summary>
        /// <remarks>
        /// Primitive transference patterns are those which have the morpheme rule differnet from epsilon
        /// E.g. it changes stative to adjunctive (A>O).
        /// </remarks>
        /// <param name="morpheme"></param>
        /// <returns></returns>
        IEnumerable<Pattern> FindPrimitiveTransferencePatterns(Morpheme morpheme);

        /// <summary>
        /// Finds modifier patterns.
        /// </summary>
        /// <param name="morpheme"></param>
        /// <returns></returns>
        IEnumerable<Pattern> FindModifierPatterns(Morpheme morpheme);

        /// <summary>
        /// All non-lexemes.
        /// </summary>
        IEnumerable<Morpheme> NonLexemes { get; }


        /// <summary>
        /// All patterns.
        /// </summary>
        IEnumerable<Pattern> Patterns { get; }

        /// <summary>
        /// Pattern graph.
        /// </summary>
        /// <remarks>
        /// How GrammarCharacters are interconnected via patterns.
        /// GrammarCharacters are nodes and patterns are edges of the graph.
        /// </remarks>
        IDirectedGraph<GrammarCharacter, Pattern> PatternGraph { get; }
    }
}
