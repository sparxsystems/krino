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



        public static EnumValue NounElement { get; } = new EnumValue(Instance);
        public static EnumValue VerbElement { get; } = new EnumValue(Instance);
        public static AdjectiveElementAttributes AdjectiveElement { get; } = new AdjectiveElementAttributes(Instance);
        public static EnumValue AdverbElement { get; } = new EnumValue(Instance);

        public static EnumValue PrepositionalPhrase { get; } = new EnumValue(Instance);
        public static EnumValue InfinitivePhrase { get; } = new EnumValue(Instance);

        
        public static EnumValue Verb { get; } = new EnumValue(Instance);
        public static ObjectAttributes Object { get; } = new ObjectAttributes(Instance);
        public static ComplementAttributes Complement { get; } = new ComplementAttributes(Instance);

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
    }
}
