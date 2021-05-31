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

        private static StructureAttributes Instance { get; } = new StructureAttributes();


        public static TermAttributes Term { get; } = new TermAttributes(Instance);
        public static ClauseAttributes Clause { get; } = new ClauseAttributes(Instance);
        public static SentenceAttributes Sentence { get; } = new SentenceAttributes(Instance);
    }
}
