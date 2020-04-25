using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributes
{
    /// <summary>
    /// Declares a structural enum with grammatical attributes for morphemes.
    /// </summary>
    public class StructAttributes : EnumRootBase
    {
        /// <summary>
        /// Hiding the constructor.
        /// </summary>
        private StructAttributes() { }

        private static StructAttributes Instance { get; } = new StructAttributes();

        /// <summary>
        /// Noun attributes.
        /// </summary>
        public static Noun Noun { get; } = new Noun(Instance);

        /// <summary>
        /// Verb attributes.
        /// </summary>
        public static Verb Verb { get; } = new Verb(Instance);

        /// <summary>
        /// Adjective attributes.
        /// </summary>
        public static Adjective Adjective { get; } = new Adjective(Instance);

        /// <summary>
        /// Adverb attributes.
        /// </summary>
        public static Adverb Adverb { get; } = new Adverb(Instance);


        /// <summary>
        /// Determiner (the word that introduces a noun) attributes.
        /// </summary>
        public static Determiner Determiner { get; } = new Determiner(Instance);

        /// <summary>
        /// Pronoun (refers to specific people or things e.g. I, me, mine, you, yours ..., they, them) attributes.
        /// </summary>
        public static Pronoun Pronoun { get; } = new Pronoun(Instance);

        /// <summary>
        /// Numeral attributes.
        /// </summary>
        public static Numeral Numeral { get; } = new Numeral(Instance);

        /// <summary>
        /// Preposition attributes.
        /// </summary>
        public static EnumValue Preposition { get; } = new EnumValue(Instance);

        /// <summary>
        /// The only postposition in english is the word ego. E.g. three years ego.
        /// </summary>
        public static EnumValue Postposition { get; } = new EnumValue(Instance);

        /// <summary>
        /// Conjunction attributes.
        /// </summary>
        public static Conjunction Conjunction { get; } = new Conjunction(Instance);

        /// <summary>
        /// Attributes of exclamation (expresses strong emotion, greeting or congratulation e.g. wonderful, Hello).
        /// </summary>
        public static EnumValue Interjection { get; } = new EnumValue(Instance);
    }
}
