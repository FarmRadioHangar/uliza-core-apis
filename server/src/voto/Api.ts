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

  export class Api {

    private url: string;

    private key: string;

    constructor(baseUrl: string = 'https://go.votomobile.org/api/v1/') {
      this.url = baseUrl.replace(/\/$/g, '');
      this.key = '';
    }

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
