using Krino.ConstructiveGrammar.Dictionary;

namespace Krino.App.Services
{
    public interface IGrammarService
    {
        Task<IConstructiveDictionary?> GetConstructiveDictionary();
    }
}
