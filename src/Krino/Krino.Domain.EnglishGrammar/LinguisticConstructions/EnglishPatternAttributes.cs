using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.LinguisticConstructions
{
    public class EnglishPatternAttributes : EnumRootBase
    {
        private EnglishPatternAttributes() { }

        private static EnglishPatternAttributes Instance { get; } = new EnglishPatternAttributes();

        public EnumValue Premise { get; } = new EnumValue(Instance);

        public EnumValue Conclusion { get; } = new EnumValue(Instance);
    }
}
