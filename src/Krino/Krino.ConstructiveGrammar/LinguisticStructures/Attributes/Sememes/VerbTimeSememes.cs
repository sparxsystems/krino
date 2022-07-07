using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes.Sememes
{
    /// <summary>
    /// Refers to when the action occurred.
    /// </summary>
    /// <remarks>
    /// Time is a concept which is related to our perception of reality. There are three times: past, present and future.
    /// </remarks>
    public class VerbTimeSememes : EnumGroupBase
    {
        public VerbTimeSememes(EnumGroupBase parent) : base(parent)
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
