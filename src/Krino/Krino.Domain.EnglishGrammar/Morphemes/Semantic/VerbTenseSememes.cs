using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Semantic
{
    /// <summary>
    /// Refers to when the action occurred.
    /// </summary>
    public class VerbTenseSememes : EnumGroupBase
    {
        public VerbTenseSememes(EnumGroupBase parent) : base(parent)
        {
            Past = new EnumValue(this);
            Present = new EnumValue(this);
            Future = new EnumValue(this);
        }

        public EnumValue Past { get; }

        public EnumValue Present { get; }

        public EnumValue Future { get; }
    }
}
