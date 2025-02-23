/**
 * simple Semaphore implementation
 */
export class Semaphore {
  private resolve?: () => void;
  private promise: Promise<void> = this.createPromise();

  private createPromise() {
    return new Promise<void>(res => {this.resolve = res});
  }

  async wait() {
    await this.promise;
  }

  signal() {
    this.resolve?.();
    this.promise = this.createPromise();
  }
}
