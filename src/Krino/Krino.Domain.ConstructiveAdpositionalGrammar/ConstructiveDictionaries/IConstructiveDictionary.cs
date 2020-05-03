using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
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
        /// Finds all lexemes matching the morph.
        /// </summary>
        /// <param name="morph"></param>
        /// <returns></returns>
        IEnumerable<IMorpheme> FindLexemes(string morph);

        /// <summary>
        /// Finds patterns matching the lexeme.
        /// </summary>
        /// <param name="lexeme"></param>
        /// <returns></returns>
        IEnumerable<IPattern> FindLexemeMatchingPatterns(IMorpheme lexeme);

        /// <summary>
        /// All patterns.
        /// </summary>
        IEnumerable<IPattern> Patterns { get; }

        /// <summary>
        /// Pattern graph.
        /// </summary>
        /// <remarks>
        /// How GrammarCharacters are interconnected via patterns.
        /// GrammarCharacters are nodes and patterns are edges of the graph.
        /// </remarks>
        IDirectedGraph<GrammarCharacter, IPattern> PatternGraph { get; }
    }
}
