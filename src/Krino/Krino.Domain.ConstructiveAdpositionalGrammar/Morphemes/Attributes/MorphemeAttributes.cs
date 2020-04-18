using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Attributes
{
    /// <summary>
    /// Declares a structural enum with grammatical attributes for morphemes.
    /// </summary>
    public class MorphemeAttributes : EnumRootGroupBase
    {
        private MorphemeAttributes() : base(11) { }

        private static MorphemeAttributes Instance { get; } = new MorphemeAttributes();

        /// <summary>
        /// Noun attributes.
        /// </summary>
        public static Noun Noun { get; } = new Noun(Instance, 1);

        /// <summary>
        /// Verb attributes.
        /// </summary>
        public static Verb Verb { get; } = new Verb(Instance, 2);

        /// <summary>
        /// Adjective attributes.
        /// </summary>
        public static Adjective Adjective { get; } = new Adjective(Instance, 3);

        /// <summary>
        /// Adverb attributes.
        /// </summary>
        public static Adverb Adverb { get; } = new Adverb(Instance, 4);


        /// <summary>
        /// Determiner (the word that introduces a noun) attributes.
        /// </summary>
        public static Determiner Determiner { get; } = new Determiner(Instance, 5);

        /// <summary>
        /// Pronoun (refers to specific people or things e.g. I, me, mine, you, yours ..., they, them) attributes.
        /// </summary>
        public static Pronoun Pronoun { get; } = new Pronoun(Instance, 6);

        /// <summary>
        /// Numeral attributes.
        /// </summary>
        public static Numeral Numeral { get; } = new Numeral(Instance, 7);

        /// <summary>
        /// Preposition attributes.
        /// </summary>
        public static EnumValue Preposition { get; } = new EnumValue(Instance, 8);

        /// <summary>
        /// The only postposition in english is the word ego. E.g. three years ego.
        /// </summary>
        public static EnumValue Postposition { get; } = new EnumValue(Instance, 9);

        /// <summary>
        /// Conjunction attributes.
        /// </summary>
        public static Conjunction Conjunction { get; } = new Conjunction(Instance, 10);

        /// <summary>
        /// Attributes of exclamation (expresses strong emotion, greeting or congratulation e.g. wonderful, Hello).
        /// </summary>
        public static EnumValue Interjection { get; } = new EnumValue(Instance, 11);
    }
}
