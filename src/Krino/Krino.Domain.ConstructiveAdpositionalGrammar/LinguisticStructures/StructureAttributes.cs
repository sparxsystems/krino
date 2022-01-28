using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Structural;
using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public class StructureAttributes : EnumRootBase
    {
        /// <summary>
        /// Hiding the constructor.
        /// </summary>
        private StructureAttributes() { }

        public static StructureAttributes Instance { get; } = new StructureAttributes();

        public static EnumValue NonLexeme { get; } = new EnumValue(Instance);
        public static EnumValue Lexeme { get; } = new EnumValue(Instance);

        public static EnumValue NounElement { get; } = new EnumValue(Instance);
        public static AdjectiveAttributes AdjectiveElement { get; } = new AdjectiveAttributes(Instance);
        public static EnumValue AdverbElement { get; } = new EnumValue(Instance);

        public static EnumValue PrepositionalPhrase { get; } = new EnumValue(Instance);

        public static EnumValue Subject { get; } = new EnumValue(Instance);
        public static EnumValue Predicate { get; } = new EnumValue(Instance);
        public static EnumValue Verb { get; } = new EnumValue(Instance);


        public static ObjectAttributes Object { get; } = new ObjectAttributes(Instance);

        public static ComplementAttributes Complement { get; } = new ComplementAttributes(Instance);


        public static ClauseAttributes Clause { get; } = new ClauseAttributes(Instance);
        public static SentenceAttributes Sentence { get; } = new SentenceAttributes(Instance);
    }
}
