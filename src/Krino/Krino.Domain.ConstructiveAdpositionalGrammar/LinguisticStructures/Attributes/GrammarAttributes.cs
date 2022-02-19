using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    public class GrammarAttributes : EnumRootBase
    {
        /// <summary>
        /// Hiding the constructor.
        /// </summary>
        private GrammarAttributes() { }

        public static GrammarAttributes Instance { get; } = new GrammarAttributes();

        public static MorphemeAttributes Morpheme { get; } = new MorphemeAttributes(Instance);

        /// <summary>
        /// Punction mark like . ! ? , ; : etc.
        /// </summary>
        public static AdPositionPunctuationMarkAttributes PunctuationMark { get; } = new AdPositionPunctuationMarkAttributes(Instance);



        public static EnumValue NounElement { get; } = new EnumValue(Instance);
        public static EnumValue VerbElement { get; } = new EnumValue(Instance);
        public static AdjectiveElementAttributes AdjectiveElement { get; } = new AdjectiveElementAttributes(Instance);
        public static EnumValue AdverbElement { get; } = new EnumValue(Instance);

        
        public static PhraseAttributes Phrase { get; } = new PhraseAttributes(Instance);

        public static ObjectAttributes Object { get; } = new ObjectAttributes(Instance);
        public static ComplementAttributes Complement { get; } = new ComplementAttributes(Instance);
        public static EnumValue AdverbialAdjunct { get; } = new EnumValue(Instance);

        /// <summary>
        /// Noun element functioning as the subject.
        /// </summary>
        public static EnumValue Subject { get; } = new EnumValue(Instance);

        /// <summary>
        /// VerbElement, Objects and complements functioning as the predicate.
        /// </summary>
        public static EnumValue Predicate { get; } = new EnumValue(Instance);


        public static ClauseAttributes Clause { get; } = new ClauseAttributes(Instance);
        public static SentenceAttributes Sentence { get; } = new SentenceAttributes(Instance);

        public static EnumValue Text { get; } = new EnumValue(Instance);


        /// <summary>
        /// Indicates a concatenation item.
        /// </summary>
        public static EnumValue Item { get; } = new EnumValue(Instance);
    }
}
