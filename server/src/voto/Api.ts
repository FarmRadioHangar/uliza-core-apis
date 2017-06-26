export namespace Voto {

  /**
   * Generic GET request parameters for interacting with the VOTO API.
   */
  interface RequestParams {

    /**
     * The maximum number of data objects that are returned in a request.
     */
    limit: number;

    /**
     * Before-threshold for traversing results that span across multiple pages.
     */
    pageBefore?: number;

    /**
     * After-threshold for traversing results that span across multiple pages.
     */
    pageAfter?: number;

  }

  /**
   * VOTO API client base class
   */
  export class Api {

    /**
     * VOTO API base url
     */
    private url: string;

    /**
     * VOTO API key
     */ 
    private key: string;

    constructor(baseUrl: string = 'https://go.votomobile.org/api/v1/') {
      this.url = baseUrl.replace(/\/$/g, '');
      this.key = process.env.VOTO_KEY;  // @TODO
    }

    /**
     * Return a well-formed url for to the provided VOTO API endpoint uri and
     * request parameters.
     *
     * @param endpoint A VOTO endpoint uri
     * @param params   GET request parameters
     *
     * @return A url which is valid for interacting with the VOTO API
     */
    public buildUrl(endpoint: string, params: RequestParams): string {
      const { limit, pageAfter, pageBefore } = params;
      let url = `${this.url}/${endpoint.replace(/^\//g, '')}?api_key=${this.key}&limit=${limit}`;
      if ('undefined' !== typeof pageAfter) {
        url += `&page_after=${pageAfter}`;
      } else if ('undefined' !== typeof pageBefore) {
        url += `&page_before=${pageBefore}`;
      }
      return url;
    }

  }

}
