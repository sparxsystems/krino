using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.SemanticAttributesArrangement
{
    /// <summary>
    /// Semantic attributes.
    /// </summary>
    public class SememeAttributes : EnumRootBase
    {
        private static SememeAttributes Instance { get; } = new SememeAttributes();


        /// <summary>
        /// Semantic attributes of verbs.
        /// </summary>
        public VerbSememeAttributes Verb { get; } = new VerbSememeAttributes(Instance);

        /// <summary>
        /// Semantic attrinutes of nouns.
        /// </summary>
        public NounSememeAttributes Noun { get; } = new NounSememeAttributes(Instance);

        /// <summary>
        /// Semantic attributes of pronouns.
        /// </summary>
        public PronounSememeAttribute Pronoun { get; } = new PronounSememeAttribute(Instance);
    }
}
